;;; .../../../var/home/fra/.doom.d/private/langs/julia.el -*- lexical-binding: t; -*-

(setq-hook! 'julia-mode-hook
  lsp-auto-guess-root nil)

(after! julia-mode
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100
                lsp-response-timeout 300)))

(use-package! lsp-julia
  :if (and (featurep! :lang julia +lsp)
	   (not (featurep! :tools lsp +eglot)))
  ;; must be set before lsp-mode is loaded
  :init
  (setq lsp-julia-response 360
	lsp-julia-timeout 360
	lsp-julia-package-dir nil)
  :config
  ;; override doom module preset
  (setq lsp-julia-default-environment
        (concat "~/.julia/environments/v"
                (shell-command-to-string "julia --version | grep -oE '[0-9]\.[0-9]'"))))

(use-package! julia-repl
  :commands julia-repl
  :config
  (if (featurep! :term vterm)
      (julia-repl-set-terminal-backend 'vterm)))

;; use it to override julia-repl julia command
;; (setq julia-repl-executable-records '((default "julia")))

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
          (org-babel-julia--goto-block-and-exec (first deps))
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


;; override julia repl fun to accept org babel :session
(after! julia-repl
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
  (cl-defun julia-repl-cd (&optional directory)
    "Change directory to the specified directory or the current buffer one (if applicable)."
    (interactive)
    (if-let ((directory (if (not directory)
                            (file-name-directory (buffer-file-name))
                          directory)))
        (progn
	  (julia-repl--send-string
           (concat
            "cd(\""
            (julia-repl--path-rewrite directory julia-repl-path-rewrite-rules)
            "\")"))
	  (with-current-buffer (julia-repl-inferior-buffer) (cd directory)))
      (warn "buffer not associated with a file"))))

(after! (julia-mode a)

  (setq-default julia-repl--session-hist (a-list))
  (define-minor-mode julia-repl-vterm-mode
    " mode for julia repl vterm buffers "
    :interactive nil)

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
             (setf (alist-get suffix julia-repl--session-hist nil) nil)))))

  (defun julia-toggle-repl-and-insert ()
    (interactive)
    (setq julia-toggle-repl--previous-window (selected-window))
    (julia-repl)
    (evil-insert nil))

  (defun julia-toggle-repl-back ()
    (interactive)
    (select-window julia-toggle-repl--previous-window))

  (defun julia-repl-send-block-string ()
    (interactive)
    (julia-repl--send-string
     (concat
      "\nbegin\n"
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
      "\nend\n")))

  (defun julia-repl-list-fields ()
    (interactive)
    (julia-repl--send-string
     (concat "fieldnames(typeof("
             (s-join "." (julia-repl--symbols-at-point)) "))")))

  (defun julia-repl-send-function ()
    " Send function at point to repl "
    (interactive)
    (save-excursion
      (julia-repl--send-string
       (buffer-substring-no-properties
        (progn (julia-beginning-of-defun) (point))
        (progn (julia-end-of-defun) (point))))))

  (if (featurep! :tools lookup +docsets)
      (set-docsets! 'julia-mode "Julia")))

(use-package! julia-snail
  :if (featurep! :lang julia +snail)
  :after julia-mode
  :commands julia-snail-mode
  :hook (julia-mode . julia-snail-mode)
  :config
  (set-lookup-handlers! 'julia-snail-mode
    :documentation #'julia-snail-doc-lookup
    :xref-backend #'xref-julia-snail)
  ;; julia-snail ancillary functions
  (load! "julia-snail")
  ;; override capf
  (setq julia-snail-mode-hook nil)
  (remove-hook 'completion-at-point-functions #'julia-snail-company-capf)
  (add-hook
   'julia-snail-mode-hook
   (lambda ()
     (remove-hook
      'completion-at-point-functions
      #'julia-snail-repl-completion-at-point)
     (add-hook 'completion-at-point-functions
               #'julia-snail-company-capf)))
  ;; don't split buffer
  (add-to-list
   'display-buffer-alist
   '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
  ;; to allow julia-snail to run in a container that binds ~/.julia
  (shell-command (concat "cp -aL "
                         (file-name-directory (locate-library "julia-snail")) " ~/.julia/packages/"))
  (defun julia-snail-load-server ()
    (interactive)
    (julia-repl--send-string
     "include(ENV[\"HOME\"] * \"/.julia/packages/julia-snail/JuliaSnail.jl\")")))

(use-package! julia-formatter
  :if (featurep! :lang julia +format)
  :commands julia-mode
  :hook (julia-mode julia-formatter--ensure-server)
  :config
  (defun julia-formatter-format-string (str)
    " Formatting function to pass to `format-all--buffer-thunk' "
    (julia-formatter--ensure-server)
    (condition-case nil
        (let* ((response (jsonrpc-request
                          julia-formatter--server-process-connection
                          :format
                          (list :text
                                (save-match-data
                                  (thread-first str
                                    (split-string  "\n" nil)
                                    (vconcat)))
                                :current_line 1))))
          (insert (mapconcat 'identity response "\n"))
          ;; success
          '(nil nil))
      ;; failure
      '(t response)))
  (set-formatter!
    'jl-julia-formatter
    #'julia-formatter-format-string
    :modes '(julia-mode))
  ;; override lsp formatting
  (setq-hook! 'julia-mode-hook +format-with-lsp nil))

;;;;
;; (set-company-backend! 'julia-mode
;;   '(:separate company-capf company-yasnippet company-dabbrev-code company-files))

(add-hook 'julia-mode-hook #'lsp)

(defun julia-repl-live-buffer ()
  (let* ((executable-key (julia-repl--get-executable-key))
         (suffix julia-repl-inferior-buffer-name-suffix)
         (terminal-backend julia-repl--terminal-backend)
         (name (julia-repl--inferior-buffer-name executable-key suffix))
         (live-buffer (julia-repl--locate-live-buffer terminal-backend name)))
    live-buffer))

(defvar julia-repl-enable-revise t "whether to use Revise automatically when repl starts")

(defun julia-repl-switch (&optional no-activate cd)
  " Enables julia repl, and activates the current project "
  (if (not (fboundp #'julia-repl-inferior-buffer))
      (require 'julia-repl))
  ;; we query for the buffer before checking for skip because we
  ;; want to switch buffer anyway
  (let ((startup (not (julia-repl-live-buffer))))
    (if (julia-repl-inferior-buffer)
        (progn
          (if (and startup (not no-activate))
              (progn
                (julia-repl-cd (projectile-project-root))
                (julia-repl-activate-parent nil)
                (when julia-repl-enable-revise
                  (julia-repl--send-string "using Revise")))
            (progn
              (when cd
                (julia-repl-cd (projectile-project-root)))
              (julia-repl)))
          t)
      nil)))


(defun julia-repl-cmd (str)
  "Send a string to julia repl switching to its buffer, if it exists."
  (when (julia-repl-switch nil t)
      (julia-repl--send-string str)))

(defun julia-franklin ()
  (interactive)
  "Start the franklin live server in the current default-dir"
  (julia-repl-cmd
   (f-read-text
    (concat
     (file-name-as-directory
      (my/script-dir #'julia-franklin)) "franklin.jl"))))

(defun julia-franklin-stop ()
  (interactive)
  (julia-repl-cmd "try Base.throwto(frank_task, InterruptException()) catch end \n"))

(after! f
  (cl-defun julia-franklin-sync-blog (&optional (src "/tmp/__site")
                                                &optional (trg (concat
                                                                (getenv "HOME")
                                                                "/dev/blog/__site.bak")))
    " Creates a symblink in target directory to src directory name if doesn't exist and syncs
the SRC folder to the TRG folder"
    (interactive)
    (let ((src-sym (concat (file-name-directory trg) (f-base src))))
      (if (or (not (file-directory-p src))
              (directory-empty-p src))
          (start-process "sync-blog-tmp" "*rsync*" "rsync"
                         "-a" (file-name-as-directory trg)
                         (file-name-as-directory src))
        (when (not (file-directory-p src))
          (error "%s exists but it's not a directory" src)))
      (if (not (and (file-symlink-p src-sym)
                    (equal (file-truename src-sym) src)))
          (if (file-exists-p src-sym)
              (error "%s exists and is not a symlink" src-sym)
            (make-symbolic-link src src-sym)))
      ;; sync from tmp to disk
      (if (file-directory-p trg)
          (start-process "sync-blog-disk" "*rsync*"
                         "rsync" "-a" (file-name-as-directory src)
                         (file-name-as-directory trg))
        (error "%s is not a valid target directory, wrong project?" trg)))))

(defun julia-franklin-publish ()
  (interactive)
  (julia-repl-cmd "pubup()"))

(defun julia-franklin-serve()
  (interactive)
  (julia-franklin-stop)
  (julia-repl-cmd "frank_task = @task serve(is_final_pass=true); schedule(frank_task)"))

(defun julia-repl-toggle-debug ()
  (interactive)
  (julia-repl-cmd "if in(\"JULIA_DEBUG\", keys(ENV))
delete!(ENV, \"JULIA_DEBUG\")
else
ENV[\"JULIA_DEBUG\"] = \"all\"
end;"))

(defun julia-repl-revise ()
  (interactive)
  (let ((thing (thing-at-point 'symbol t)))
    (julia-repl-cmd
     (format
      "(isdefined(Main, :%s) && isa(%s, Module)) ? revise(%s) : revise()"
      thing thing thing))))

;; julia projects file
(after! projectile
  (appendq! projectile-project-root-files '("Project.toml" "JuliaProject.toml")))

(set-file-template! ".*/blog/posts/.+\\.md$" :trigger "blog_post" :project t)
(set-docsets! 'julia-repl-vterm-mode :add "Julia")
