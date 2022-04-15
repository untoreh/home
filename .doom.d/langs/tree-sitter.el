;;; langs/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :when (and nil bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (setq ;; tree-sitter-langs-grammar-dir "/usr/lib/tree-sitter"
        tsc-dyn-get-from '(:compilation))
  (let ((rflags (getenv "RUSTFLAGS")))
    (defadvice! tree-sitter-set-flags nil :before #'tsc-dyn-get--build
      (setenv "RUSTFLAGS" "-C target-feature=-crt-static"))
    (defadvice! tree-sitter-unset-flags nil :after #'tsc-dyn-get--build
      (setenv "RUSTFLAGS" rflags))
    (defadvice! tree-sitter-bin-dir nil :override #'tree-sitter-langs--bin-dir
      "/usr/lib/tree-sitter")
    (defadvice! skip-grammars (&rest args) :override #'tree-sitter-langs-install-grammars nil))
  :config
  (require 'tree-sitter-langs)
  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
	(apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
				       "^No language registered\\|"
				       "cannot open shared object file")
			       (error-message-string e))
	 (signal (car e) (cadr e)))))))
