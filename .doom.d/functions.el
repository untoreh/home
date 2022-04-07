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

(defun my/force-kill-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (if-let* ((proc (get-buffer-process (current-buffer)))
                (pid (process-id proc)))
          (shell-command (concat "pkill -P " (number-to-string pid))))
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

(provide 'functions)
(require'functions)

;;; functions.el ends here
;;;
