;;; ../../../var/home/fra/.doom.d/langs/julia-snail.el -*- lexical-binding: t; -*-

(defun julia-snail--query-server (query sym)
  (let* ((module (julia-snail--module-at-point))
         (name (s-concat (s-join "." module) "." sym))
         (res (julia-snail--send-to-server
               :Main
               (format query name)
               :display-error-buffer-on-failure? nil
               :async nil)))
    res))

(defun julia-snail--company-doc-buffer (str)
  (let ((doc (julia-snail--query-server "@doc %s" str)))
    (let ((buf (julia-snail--message-buffer
                julia-snail-repl-buffer
                "doc-buffer"
                (if (eq :nothing doc)
                    "Documentation not found!\nDouble-check your package activation and imports."
                  doc)
                :markdown nil)))
      (with-current-buffer buf
        (font-lock-ensure))
      buf)))

(defun julia-snail--company-annotation (str)
  (let ((res (julia-snail--query-server "methods(%s).ms[1].sig" str)))
    (if (eq res :nothing)
        nil
      res)))

(defun julia-snail-company-capf ()
  (interactive)
  (let* ((comp (julia-snail-repl-completion-at-point))
         (doc (list :company-doc-buffer
                    #'julia-snail--company-doc-buffer))
         (ann (list :annotation-function
                    #'julia-snail--company-annotation)))
    (concatenate 'list comp doc ann)))

(use-package! julia-snail
  :if (modulep! :lang julia +snail)
  :after julia-mode
  :commands julia-snail-mode
  :hook (julia-mode . julia-snail-mode)
  :config
  (set-lookup-handlers! 'julia-snail-mode
    :documentation #'julia-snail-doc-lookup
    :xref-backend #'xref-julia-snail)
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
