;;; langs/julia-repl.el -*- lexical-binding: t; -*-

(use-package! julia-repl
  :init
  :config
  (after! lsp-julia
    (pushnew! julia-repl-executable-records
              `(compiled ,lsp-julia-command)
              `(trickle ,(executable-find "trickle-julia")))
    (setq julia-repl-executable-key 'trickle))
  (set-docsets! 'julia-repl-vterm-mode :add "Julia")
  (if (modulep! :term vterm)
      (julia-repl-set-terminal-backend 'vterm))
  ;; (setq julia-repl-switches "--optimize=0 --compile=min")
  (setq julia-repl-switches "")
  ;; instead of looking at the innermost project, activate the one from projectile
  (defadvice! julia-repl-activate-main-proj (orig-fn &rest args)
    :around #'julia-repl-activate-parent
    (letf! ((defun locate-dominating-file (&rest args) (projectile-project-root)))
      (apply orig-fn args)))
  (setq-hook! 'julia-mode julia-repl-inferior-buffer-name-suffix (projectile-project-root)))

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

;; Ensure that julia always has the `--project' flag. This is important for when
;; the julia startup file (`~/.julia/config/startup.jl') loads precompiled code
;; that is project specific.
(defadvice! julia-repl-ensure-project-arg (orig-fn &rest args)
  :around #'julia-repl-inferior-buffer
  (let ((julia-repl-switches (if projflag
                                 (progn
                                   (concat julia-repl-switches "--project=\""
                                           (file-truename (projectile-project-root)) "\""))
                               julia-repl-switches)))
    (apply orig-fn args)))

(defun julia-repl-startup ()
  (interactive)
  (julia-repl-cd (projectile-project-root))
  (ignore-errors (julia-repl-activate-parent nil))
  (let ((include-begin (concat "include(\""
                               (file-name-as-directory
                                (my/script-dir #'julia-franklin)))))
    (when julia-repl-enable-revise
      (julia-repl-cmd "revise!()"))
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
              (julia-repl-startup nil)
            (progn
              (when cd
                (julia-repl-cd (projectile-project-root)))
              (julia-repl))

            )
          t)
      nil)))

(require 'aio)
(aio-defun julia-repl-cmd (str &optional wait cd)
  "Send a string to julia repl switching to its buffer, if it exists."
  (setq julia-toggle-repl--previous-window (selected-window))
  (when (julia-repl-switch nil cd)
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

(defvar julia-repl-test-args "" "Arguments passed to the Pkg `test' command.")
(make-variable-buffer-local 'julia-repl-test-args)
(defun julia-repl-run-tests (targets)
  "Add debug packages to the current LOAD_PATH."
  (interactive (list (read-string "targets: " julia-repl-test-args)))
  (let* ((test-args (split-string targets "," t " +"))
         (quoted-args (mapconcat (lambda! (x) (format "\"%s\"" x)) test-args "")))
    (julia-repl-cmd (format "import Pkg; Pkg.test(;test_args=[%s])" quoted-args))
    (setq-local julia-repl-test-args targets)))

(defvar julia-repl-follow-buffer nil "When enabled, tail the repl buffer until the prompt is shown again.")
(defun julia-repl-reset-cursor(match &rest forms)
  (when (or (not julia-repl-follow-buffer)
            (progn
              (goto-char  (point-max))
              (while (looking-at  "^$")
                (forward-line -1)
                (beginning-of-line))
              (looking-at match)))
    forms
    ))

;; The `debug!' should be defined in ~/.julia/config/startup.jl
(defun julia-repl-toggle-debug ()
  (interactive)
  ;; remove 2 lines from repl history
  (julia-repl-cmd "debug!(2)"))

(defun julia-repl-revise ()
  (interactive)
  (julia-repl-cmd "Revise.retry()")
  (evil-insert nil)
  (julia-toggle-repl-back))

(defun julia-repl-revise-at-point ()
  "Revise thing at point."
  (interactive)
  (let ((thing (thing-at-point 'symbol t)))
    (julia-repl-cmd
     (format
      "(isdefined(Main, :%s) && isa(%s, Module)) ? revise(%s) : revise()"
      thing thing thing))))

;; allows subprocesses to inherit the env vars
(after! envrc
  (inheritenv-add-advice #'julia-repl-inferior-buffer))
