;;; langs/julia-ob.el -*- lexical-binding: t; -*-
;;;
;; override julia repl fun to accept org babel :session
(after! (julia-repl ob)
  ;; (setq julia-repl-executable-records '((default "julia")))
  (fset 'julia-repl--inferior-buffer-name 'nil)
  (cl-defun julia-repl--inferior-buffer-name
      (&optional (executable-key (julia-repl--get-executable-key))
                 (suffix julia-repl-inferior-buffer-name-suffix))
    (let* ((middle (if (equal executable-key (julia-repl--default-executable-key))
                       ""
                     (format "-%s" executable-key)))

           (last (cond
                  ((null suffix) "")
                  ((integerp suffix) (format "<%d>" suffix))
                  ((symbolp suffix) (format "-%s" suffix))
                  (t (error
                      "Inferior name suffix should be an integer or a symbol")))))

      (concat julia-repl-inferior-buffer-name-base middle last)))
  (add-hook!
   'julia-repl-hook
   ;; for keybindings between blocks and repls
   #'(lambda ()
       (with-current-buffer (julia-repl-inferior-buffer)
         (julia-repl-vterm-mode)))
   ;; reset org babel session history
   #'(lambda ()
       (let ((suffix (symbol-name julia-repl-inferior-buffer-name-suffix)))
         (if (not (alist-get suffix julia-repl--session-hist))
             (setf (alist-get suffix julia-repl--session-hist nil) nil))))))

(after! (or ob ob-julia)
  ;; make sure to load ob-julia before, to override funcs
  (require 'ob-julia)
  (setq-default julia-repl--buffers-list nil
                julia-repl--call-stack nil
                julia-repl--call-count 0)
  ;; vterm support for julia repl
  ;; (defun julia-repl--vterm-prompt ()
  ;;   (julia-repl--send-string
  ;;    "import OhMyREPL; OhMyREPL.input_prompt!(\"\\e\]51\;A\\e\\\\julia>\")"))
  ;; (add-to-list julia-repl-hook #'julia-repl--vterm-prompt)

  (defun org-babel-julia--goto-block-and-exec (name)
    (save-excursion
      (org-babel-goto-named-src-block name)
      (org-babel-execute-src-block)))

  (defun org-babel-julia--push-body-maybe (body params)
    " Queue src block body eval according to cache and repl instance "
    (let* ((session (cdr (assoc :session params)))
           (block-hash (md5 body))
           (session-hist (assoc session
                                julia-repl--session-hist))
           (hist (cdr session-hist))
           (once (assoc :once params)))

      ;; only run block once (per session) if :once header is present
      (if (or (not once) (not (member block-hash hist)))
          (progn
            (push (org-babel-expand-body:julia body params)
                  julia-repl--call-stack)
            ;; this list grows indefinitely since we don't identify blocks
            (push block-hash hist)
            (if once
                (if session-hist
                    (setf (cdr (assoc session julia-repl--session-hist)) hist)
                  (push (cons session hist) julia-repl--session-hist)))))))

  (require 'aio)
  (aio-defun org-babel-julia--send-string (str)
    ;; this function must be called within the terminal buffer context
    ;; because aio doesn't support buffer context switching
    (if (not vterm--term)
        (error "Not in a vterm buffer when executing julia src block"))
    (while (eq 0 (current-column))
      (progn
        (message "waiting for repl prompt...")
        (vterm-reset-cursor-point)
        (aio-await (aio-sleep 0.25))))
    (julia-repl--send-string str))

  (defun org-babel-julia--execute-src-block (body params)
    (interactive)
    (cl-incf julia-repl--call-count)
    (let ((deps (split-string (or (alist-get :deps params) ""))))
      (if (eq 1 (length deps))
          (org-babel-julia--goto-block-and-exec (cl-first deps))
        (map 'list #'org-babel-julia--goto-block-and-exec deps)))

    ;; set state to block headers
    (setq
     julia-repl-inferior-buffer-name-suffix
     (intern (cdr (assq :session params)))
     julia-repl-executable-key
     (intern (or (cdr (assq :exec params)) "default")))

    ;; call to spawn the repl if it doesn't exist
    (julia-repl-inferior-buffer)

    ;; add new buffer (if new) to buffers list
    ;; and clear its session reference history
    (let* ((session (alist-get :session params))
           (ib-name (julia-repl--inferior-buffer-name))
           (inferior-buffer (get-buffer
                             (julia-repl--add-earmuffs
                              ib-name))))

      (if (not (member inferior-buffer julia-repl--buffers-list))
          (progn
            (push inferior-buffer julia-repl--buffers-list)
            (assoc-delete-all session julia-repl--session-hist))))
    ;; add block body respecting headers
    (org-babel-julia--push-body-maybe body params)
    ;; only send string if at the bottom of call stack
    (if (eq julia-repl--call-count 1)
        (let ((stack julia-repl--call-stack))
          (with-current-buffer (julia-repl-inferior-buffer)
            (org-babel-julia--send-string
             (concat "\nbegin\n"
                     (string-join (reverse stack) "\nend\nbegin\n")
                     (if (member "silent" (split-string (rest (assoc :results params))))
                         "\nnothing")
                     "\nend\n"))
            ;; (message "sent block to julia repl")
            )))
    (cl-decf julia-repl--call-count))
  ;; wrap execute src block to reset recursion
  (defun julia-repl-reset-and-execute-src-block ()
    (interactive)
    (setq julia-repl--call-stack nil
          julia-repl--call-count 0)
    (org-babel-execute-src-block))
  ;; override ob-julia
  (fset #'org-babel-execute:julia nil)
  (fset #'org-babel-execute:julia #'org-babel-julia--execute-src-block)
  (fset #'org-babel-edit-prep:julia (lambda (&rest args)))
  (fset #'org-babel-julia-initiate-session (lambda (&rest args))))
