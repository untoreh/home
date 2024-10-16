;; -*- lexical-binding: t; -*-
;;

(defun my/script-dir (&optional obj)
  " Returns the path of the function OBJ or the path where
`my/script-dir' is defined."
  (if (and load-file-name (null obj))
      (file-name-directory load-file-name)
    (f-dirname (cdr (find-function-library (or obj #'my/script-dir))))))

(defmacro comp-defun (name args &rest body)
  `(progn
     (cl-defun ,name ,args ,@body)
     (native-compile #',name)))

;; (comp-defun evil-duplicate-line ()
;;             (evil-next-line-1-first-non-blank)
;;             (evil-visual-char)
;;             (evil-end-of-line)
;;             (evil-yank)
;;             (evil-open-below)
;;             (evil-paste)
;;             )

(defmacro my/delete! (elt list)
  "`delete' ELT from LIST in-place."
  `(setq ,list (delete ,elt ,list)))

(defmacro my/concatq! (place &rest forms)
  "`concat' all FORMS with value in PLACE updating PLACE."
  `(setq ,place (concat ,place ,@forms)))

(defun my/force-kill-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (if-let* ((proc (get-buffer-process (current-buffer)))
                (pid (process-id proc)))
          (shell-command (concat "sudo pkill -P " (number-to-string pid))))
      (kill-buffer))))

(defun my/float-to-floor-precision (num prec)
  " TODO: is this type stable? "
  (string-to-number
   (format (concat "%." (number-to-string prec) "f")
           num)))

(defmacro with-no-gc (body)
  " Evaluate BODY ensuring no garbage collection occurs "
  `(let ((gcfun (symbol-function #'garbage-collect))
         (gc-cons-threshold most-positive-fixnum))
     (fset #'garbage-collect (lambda))
     (unwind-protect
         ,body
       (fset #'garbage-collect gcfun))))

;; recursive version of my/first-common-prefix
;; (cl-defun my/first-common-prefix (candidates &optional (min-length 2))
;;   (let ((common (try-completion "" candidates)))
;;     (if (not (eq "" common))
;;         common
;;       (my/first-common-prefix (butlast candidates) min-length))))

(cl-defun my/first-common-prefix (candidates)
  (let* ((can (seq-into candidates 'vector))
         (n (length can))
         (match ""))
    (cl-dotimes (i (- n 1))
      (setf match (try-completion "" (seq-into (seq-subseq can i (+ i 2)) 'list)))
      (when (not (eq match ""))
        (cl-return match)))
    (if (not (eq "" match))
        match
      nil)))

(defun my/company-update-first-common (&rest _)
  (setq company-common (my/first-common-prefix company-candidates)))

(defun my/ensure-symlink (trg name)
  (let ((orig name)
        (trg (file-truename trg))
        (name (file-truename name)))
    (cond
     ((not (file-exists-p name))
      (progn
        (and (file-symlink-p name) (delete-file name))
        (make-directory (file-name-directory name) t)
        (make-symbolic-link trg name)))
     ((and (file-symlink-p name)
           (not (equal (file-truename trg)
                       (file-truename name))))
      (progn
        (delete-file name)
        (make-symbolic-link trg name)))
     ((and (file-exists-p name)
           (not (file-symlink-p orig))) (error "%s is not a symlink" orig))
     )))

(require 'dash)
(defun my/concat-path (&rest parts)
  (-reduce (lambda (a b) (expand-file-name b a)) parts))

;; FIXME: functions.el file not found error given by `native-compile-async'... `native-compile' works fine...
                                        ;(after! find-func
                                        ;(let ((filepath (cdr (find-function-library #'my/script-dir))))
                                        ;(native-compile-async filepath t nil)))

(defun my/select-first (f seq)
  (catch 'found
    (mapc (lambda (x)
            (when (funcall f x)
              (throw 'found x))
            ) seq)
    nil))

(after! vterm
  ;; vterm shell command
  (defun my/run-in-vterm-kill (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b)
           (kill-buffer b))))
  (defun my/run-in-vterm (command)
    "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
    (interactive
     (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
             (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
        (read-shell-command "Terminal command: "
                            (cons filename 0)
                            (cons 'shell-command-history 1)
                            (list filename)))))
    (with-current-buffer (vterm (concat "*" command "*"))
      (set-process-sentinel vterm--process #'my/run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return)))
  (defun my/repl-vterm-bufferp (&rest args)
    "Check if the current buffer is a repl vterm buffer of any language in `enabled-langs'."
    (catch 'enabled
      (mapc (lambda (l)
              (let ((mode (intern (concat
                                   (symbol-name l)
                                   "-repl-vterm-mode"))))
                (when (and (boundp mode)
                           (symbol-value mode))
                  (throw 'enabled t))))
            enabled-langs)
      nil))

  )

(defconst my/print-funcs-julia '("display(%s)" "Main.display!(%s)" "@info %s" "@linfo %s") "Print functions for julia")
(defvar my/print-funcs-current-julia "@info %s" "Current print function for julia")

(use-package! rotate-text
  :config
  (defun my/rotate-print-func (lang)
    (let ((varname (intern (concat "my/print-funcs-current-" (symbol-name lang))))
          (words (intern (concat "my/print-funcs-" (symbol-name lang)))))
      (set varname (rotate-text-replacement (symbol-value words) (symbol-value varname) 1))
      )))

(defun my/mode-print-cmd ()
  (pcase major-mode
    ('nim-mode "echo %s")
    ('emacs-lisp-mode "(prin1 %s)")
    ('python-mode "print(%s)")
    ((or 'js-mode 'rjsx-mode) "console.log(%s)")
    ((or 'julia-mode 'julia-ts-mode) my/print-funcs-current-julia) ;; display! is defined in ~/.julia/config/startup.jl
    ('sh-mode "echo %s")
    ('rustic-mode "println!(\"{}\", %s);")
    (m (error "No print command found for major mode %s" m))
    ))

(defun my/prepend-file-name (suffix)
  (format "\"%s:%d\"" (file-name-nondirectory buffer-file-name) suffix))

(defun my/logstring-kind (kind)
  (pcase kind
    ;; last yanked string
    ('yank (substring-no-properties (car kill-ring-yank-pointer)))
    ;; line number
    ('nil (my/prepend-file-name (+ 1 (line-number-at-pos))))))

;; NOTE: this should be a rgx
;; (defun char-upcase-p (c)
;;   (equal c (upcase c)))
;; (defun my/split-string-titlecase (s)
;;   (let ((i 0)
;;         (idx '())
;;         (res '())
;;         )
;;     (mapc
;;      (lambda (c)
;;        (when (char-upcase-p c)
;;          (push i idx))
;;        (cl-incf i))
;;      (split-string s "" t))
;;     (setq idx (reverse idx))
;;     (when (equal 0 (cl-first idx))
;;       (pop idx))
;;     (let ((pos 0))
;;       (mapc (lambda (i)
;;               (push (substring s pos i) res)
;;               (setq pos i)) idx)
;;       (push (substring s pos (length s)) res))
;;     (reverse res)))

(defvar my/insert-print-list nil "Buffer local list of currently inserted print statements")
(make-variable-buffer-local 'my/insert-print-list)

(defun my/insert-print (&optional kind)
  "Insert a print statements at point."
  (interactive)
  (let* ((cmd (my/mode-print-cmd))
         (logstring (my/logstring-kind kind))
         (logcmd (format cmd logstring)))
    (evil-open-below 1)
    (insert logcmd)
    (evil-escape)
    (push logcmd my/insert-print-list)))

(defun my/insert-print-clear ()
  "Delete all lines previously inserted."
  (interactive)
  ;; Should delete from from the last inserted to the first one. Since we `push'
  ;; the order is correct.
  (save-excursion
    (let ((cleaned 0)
          (todo (length my/insert-print-list)))
      (while my/insert-print-list
        (catch 'not-found
          (goto-char (point-max))
          (let ((logstring (pop my/insert-print-list)))
            (if-let ((beg (search-backward logstring nil t)))
                (progn (delete-char (length logstring))
                       (delete-blank-lines)
                       ))))))))

(defun my/major-mode-lang ()
  (intern (cl-first (split-string (symbol-name major-mode) "-"))))

(defun my/lang-major-mode (lang)
  (cond lang
        (((symbolp lang) (intern (concat (symbol-name lang) "-mode"))))
        (((stringp lang) (intern (concat lang "-mode"))))
        ))

(map!
 (:prefix "SPC i l"
  :desc "insert num log"
  :nv "l"
  #'my/insert-print)
 (:prefix "SPC i l"
  :desc "insert yank log"
  :nv "y"
  (cmd! (my/insert-print 'yank)))
 (:prefix "SPC i l"
  :desc "rotate print function"
  :nv "r"
  (cmd! (my/rotate-print-func (my/major-mode-lang))))
 (:leader
  :desc "clear log strings"
  :nvi "d l"
  #'my/insert-print-clear)
 )

(defun my/tail-f-window (&rest _)
  "Go to the end of Messages buffer."
  (let ((window (get-buffer-window (buffer-name))))
    (with-current-buffer (window-buffer window)
      (set-window-point window (point-max)))))

(defun my/ts-major-mode-p (mode-symbol)
  "Check if MODE-SYMBOL has a defined tree sitter major mode."
  (let* ((name (symbol-name mode-symbol))
         (major-mode-symbol (intern
                             (substring
                              name 0
                              (if (string-suffix-p "-ts-mode" name) 8 (if (> (length name) 5) 5 (length name)))))))
    (and (symbolp major-mode-symbol)
         (let ((ts-mode-symbol (intern (concat (symbol-name major-mode-symbol) "-ts-mode"))))
           (if (fboundp ts-mode-symbol)
               ts-mode-symbol
             nil)))))

(defun my/port-openp (host port)
  "Test if PORT is open on HOST."
  (condition-case nil
      (progn
        (delete-process (open-network-stream "test" nil host port))
        t)
    (file-error
     nil)))


(provide 'functions)
(require 'functions)

;;; functions.el ends here
;;;
