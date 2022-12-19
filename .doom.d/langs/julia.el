;;; .../../../var/home/fra/.doom.d/private/langs/julia.el -*- lexical-binding: t; -*-

;; TODO: add to julia-repl major mode hook, to set buffer name suffix, to avoid killing the vterm buffer when switchign projects
;;

(setq-hook! 'julia-mode-hook
  lsp-auto-guess-root nil)

(after! julia-mode
  (when (modulep! :lang julia +lsp)
    (add-hook! julia-mode #'lsp))
  (add-hook! 'julia-mode-hook
    (setenv "JULIA_NUM_THREADS" (number-to-string (num-processors)))
    ;; https://github.com/doomemacs/doomemacs/commit/acae9f9acb328c46f71b4cc975abcdb95c09cee6
    ;; (setq-local lsp-enable-folding t
    ;;             lsp-folding-range-limit 100
    ;;             lsp-response-timeout 300)
    ))

(use-package! lsp-julia
  :if (and (modulep! :lang julia +lsp)
	   (not (modulep! :tools lsp +eglot)))
  ;; must be set before lsp-mode is loaded
  :init
  (setq lsp-julia-response 360
	lsp-julia-timeout 360
	lsp-julia-package-dir nil)
  :config
  ;; override doom module preset
  (setq lsp-julia-default-environment
        (concat "~/.julia/environments/v"
                (s-chomp (shell-command-to-string "julia --version | grep -oE '[0-9]\.[0-9]'")))))

(use-package! julia-repl
  :commands julia-repl
  :config
  (if (modulep! :term vterm)
      (julia-repl-set-terminal-backend 'vterm))
  ;; (setq julia-repl-switches "--optimize=0 --compile=min")
  (setq julia-repl-switches "--optimize=0")
  )

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

;; override julia repl fun to accept org babel :session
(after! (julia-repl ob)
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

;; functions to send code to repl
(after! (julia-mode julia-repl)
  ;; (require 'a)
  (setq-default julia-repl--session-hist (copy-alist '()))
  (define-minor-mode julia-repl-vterm-mode
    " mode for julia repl vterm buffers "
    :interactive nil)

  (cl-defun julia-repl-cd (&optional directory)
    "Change directory to the specified directory or the current buffer one (if applicable)."
    (interactive)
    (if-let ((directory (if (not directory)
                            (file-name-directory (or (buffer-file-name) dired-directory))
                          directory)))
        (progn
	  (julia-repl--send-string
           (concat
            "cd(\""
            (julia-repl--path-rewrite directory julia-repl-path-rewrite-rules)
            "\")"))
	  (with-current-buffer (julia-repl-inferior-buffer) (cd directory)))
      (warn "buffer not associated with a file")))

  (defun julia-repl-buffer-p (&rest args) julia-repl-vterm-mode)
  (after! emojify
    (pushnew! emojify-inhibit-in-buffer-functions #'julia-repl-buffer-p))

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

  (if (modulep! :tools lookup +docsets)
      (set-docsets! 'julia-mode "Julia")))

(use-package! julia-snail
  :if (modulep! :lang julia +snail)
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
  :if (modulep! :lang julia +format)
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
(defvar julia-repl-enable-snoop nil "whether to use SnoopCompile automatically when repl starts")
(defvar julia-repl-dir nil "The directory that holds the package.")

(defun julia-repl-send-file (filename)
  (let ((include-begin
         (or julia-repl-dir
             (let ((incl (concat
                          "include(\""
                          (file-name-as-directory
                           (my/script-dir #'julia-repl-send-file)))))
               (setq julia-repl-dir incl)
               incl))))
    (julia-repl--send-string
     (concat include-begin filename "\")" ))))

(defun julia-repl-startup ()
  (interactive)
  (julia-repl-cd (projectile-project-root))
  (ignore-errors (julia-repl-activate-parent nil))
  (let ((include-begin (concat "include(\""
                               (file-name-as-directory
                                (my/script-dir #'julia-franklin)))))
    (when julia-repl-enable-revise
      (julia-repl-send-file "revise.jl"))
    (when julia-repl-enable-snoop
      (julia-repl-send-file "snoop.jl"))))

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
              (julia-repl-startup)
            (progn
              (when cd
                (julia-repl-cd (projectile-project-root)))
              (julia-repl)))
          t)
      nil)))

(require 'aio)
(aio-defun julia-repl-cmd (str &optional wait)
  "Send a string to julia repl switching to its buffer, if it exists."
  (when (julia-repl-switch nil t)
    (when (and wait (> wait 0))
      (aio-await (aio-sleep wait))) ;; HACK: allow ohmyrepl.jl to load (this prevents extra "]" being inserted)
    (julia-repl--send-string str)))

(aio-defun julia-repl-precompile ()
  "Precompile current active project."
  (when (julia-repl-switch nil nil)
    (vterm-send-backspace)
    (aio-await (aio-sleep 0.1))
    (aio-await (julia-repl-cmd "import Pkg; Pkg.precompile()\n"))
    ))

(defun julia-repl--push-load-path (pkg)
  (let* ((path (concat "\"" "$(DEPOT_PATH[1])/packages/" pkg "\""))
         (most-recent ( ))
         (cmd (concat "!(" path " in LOAD_PATH) && " "push!(LOAD_PATH, " path ")")))
    (aio-wait-for (julia-repl-cmd cmd))
    ))

(defun julia-repl-debug-packages ()
  "Add debug packages to the current LOAD_PATH."
  (julia-repl-send-file "debug_packages.jl"))

(defconst julia--regexp-struct (rxt-pcre-to-elisp  "^struct\s"))
(defconst julia--regexp-proto-struct (rxt-pcre-to-elisp  "^@proto struct\s"))
(defconst julia--regexp-proto-module (rxt-pcre-to-elisp  "^using ProtoStructs\n"))
(defun julia--proto-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward julia--regexp-proto-module nil t)
    ))
(defun julia--protify (&optional flag)
  (interactive)
  (if (equal major-mode #'julia-mode)
      (save-excursion
        (goto-char (point-min))
        (if flag
            (progn
              (insert "using ProtoStructs\n")
              (while (re-search-forward  julia--regexp-struct nil t)
                (replace-match "@proto struct ")))
          (progn
            (while (re-search-forward  julia--regexp-proto-module nil t)
              (replace-match ""))
            (while (re-search-forward  julia--regexp-proto-struct nil t)
              (replace-match "struct "))))
        (save-buffer))
    (warn "Current buffer is not a julia buffer.")))

(defun julia-toggle-proto-structs ()
  (if (julia--proto-p)
      (julia--protify)
    (julia--protify t)
    )
  )

(defun julia-deprotify-structs ()
  (if (equal major-mode #'julia-mode)
      (save-excursion
        (goto-char (point-min))
        (insert "using ProtoStructs\n")
        (replace-regexp  julia-regexp-struct "@proto struct ")
        )
    (warn "Current buffer is not a julia buffer."))
  )

(aio-defun julia-franklin ()
  (interactive)
  "Start the franklin live server in the current default-dir"
  (julia-repl-cmd
   (f-read-text
    (concat
     (file-name-as-directory
      (my/script-dir #'julia-franklin)) "franklin.jl"))
   1) ;; wait at startup
  (vterm-send-return))

(defvar julia-repl-follow-buffer nil "When enabled, tail the repl buffer until the prompt is shown again.")
(defun vterm-maybe-reset-cursor(match &rest forms)
  (when (or (not julia-repl-follow-buffer)
            (progn
              (goto-char  (point-max))
              (while (looking-at  "^$")
                (forward-line -1)
                (beginning-of-line))
              (looking-at match)))
    forms
    ))
(aio-defun julia-franklin-maybe-stop ()
  (catch 'stop
    (julia-repl-switch t nil)
    (let ((buf (current-buffer)))
      (while t
        (unwind-protect
            (progn
              (switch-to-buffer (julia-repl-live-buffer))
              (vterm-maybe-reset-cursor
               ".*Use Pkg.activate() to go back"
               (vterm-send-C-c)
               (throw 'stop t))
              (vterm--maybe-reset-cursor
               (concat julia-prompt-regexp "$")
               (throw 'stop t)))
          (switch-to-buffer buf)
          (aio-await (aio-sleep 0.2))
          (switch-to-buffer buf)
          )))))

(defun julia-franklin-stop ()
  (interactive)
  (julia-repl-cmd "try Base.throwto(frank_task, InterruptException()) catch end")
  (julia-franklin-maybe-stop))

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
  (julia-franklin-stop)
  (julia-repl-cmd "pubup()"))

(defun julia-franklin-serve()
  (interactive)
  (julia-franklin-stop)
  (julia-repl-cmd "frank_task = @task serve(prerender=true, is_final_pass=true); schedule(frank_task)")
  (vterm-send-return))

;; The `debug!' should be defined in ~/.julia/config/startup.jl
(defun julia-repl-toggle-debug ()
  (interactive)
  ;; remove 2 lines from repl history
  (julia-repl-cmd "debug!(2)"))

(defun julia-repl-revise-at-point ()
  "Revise thing at point."
  (interactive)
  (let ((thing (thing-at-point 'symbol t)))
    (julia-repl-cmd
     (format
      "(isdefined(Main, :%s) && isa(%s, Module)) ? revise(%s) : revise()"
      thing thing thing))))

;; julia projects file
(after! projectile
  (appendq! projectile-project-root-files '("Project.toml" "JuliaProject.toml"))
  (setq-hook! 'julia-mode-hook projectile-project-test-cmd
              "julia --startup-file=no --project=test/ test/runtests.jl --test-args '' ")
  )

;;; add tag completion to blog posts
(after! corfu
  (defconst
    blog-tags-list
    (with-temp-buffer
      (insert-file-contents
       (my/concat-path doom-private-dir
                       "langs"
                       "blog_tags.txt"))
      (split-string (buffer-string)))
    "The list of tags for the blog tags backend.")
  (defun blog-tags-capf ()
    "Completes based on a list of tags defined in `company-blog-tags'."
    (interactive)
    (let* ((start (line-beginning-position))
           (beg (buffer-substring-no-properties start (min (buffer-size) (+ start 6))))
           )
      (when (equal beg "tags =")
        (let* ((pos (point))
               (word (word-at-point))
               (beg (- pos (length word)))
               (end pos)
               )
          (list beg pos blog-tags-list . nil)
          ))
      )
    )

  (defvar my/prev-capfs nil "Stores previous company backends in buffer.")
  (define-minor-mode blog-tags-mode
    "Minor mode for blog tags completion."
    :lighter "blog-tags-capf"
    :global nil
    (if blog-tags-mode
        (progn
          (setq-local my/prev-capfs completion-at-point-functions)
          (setq-local completion-at-point-functions '(blog-tags-capf))
          )
      ))

  ;; (add-hook 'gfm-mode-hook (lambda ()
  ;;                            (and (string-match ".*/dev/blog/.*" (buffer-file-name))
  ;;                                 (blog-tags-mode))))

  (map! :mode gfm-mode
        :localleader
        :desc "Toggle completion of blog tags."
        :nv "t" (cmd! (blog-tags-mode (if blog-tags-mode -1 1)))
        )
  )

(add-transient-hook! 'julia-repl-mode-hook
  (set-popup-rule! "^\\*julia\\*" :height 25 :quit t :select nil))
(set-file-template! ".*/blog/posts/.+\\.md$" :trigger "blog_post" :project t)
(after! julia-repl
  (set-docsets! 'julia-repl-vterm-mode :add "Julia"))
