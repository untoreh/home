;;; langs/gpt.el -*- lexical-binding: t; -*-

(use-package! aichat
  :config
  (setq
   aichat-bingai-cookies-file (my/concat-path doom-user-dir "cache" "bing.json")
   aichat-bingai-assistant-buffer "*bingai*"
   aichat-bingai-conversation-style 'creative
   aichat-bingai-chat-file (my/concat-path temporary-file-directory "chatbot-history")
   )
  (aichat-bingai-prompt-create "tests-generator"
                               :input-prompt "Please write tests for: "
                               :text-format "Please identify the programming language in the input. Your answer must only contain the unit tests that test the input code. The following is the code I want you to write tests for:\n%s"
                               :assistant t)

  (defvar-local docsgen-local-prompt "")
  ;; (setq aichat-bingai-convert-to-org-default (symbol-function #'aichat-bingai--chat-convert-to-org))
  ;; (setf aichat-bingai--chat-convert-to-org (lambda (&rest args)))
  ;; (setf aichat-bingai--chat-convert-to-org aichat-bingai-convert-to-org-default))
  (defun my/local-docsgen-prompt ()
    (interactive)
    (setq docsgen-local-prompt (format "
The answer must only contain the code with the updated documentation.
The function signature must specify the full types if they are inferrable from the function body.
When using function or variable names inside the documentation strings ensure to enclose them in escaped '`' ticks.
Use the proper enclosing characthers to insert documentation according to the syntax rules of the language.
Don't spend too many words on small functions, only provide examples for long and complex functions.
The following is the code that should be documented:
```%s
%s
```" (my/major-mode-lang) "%s"))
    (aichat-bingai-prompt-create "docs-generator"
                                 :input-prompt "Please document this code: "
                                 :text-format docsgen-local-prompt
                                 :chat t
                                 :assistant t))
  (my/local-docsgen-prompt)
  (map! (:prefix ("SPC i c" . "ChatBot")
         :desc "chat"
         :nev "c" #'aichat-bingai-chat
         :desc "chat (from scratch)"
         :nev "C" (cmd! (aichat-bingai-conversation-reset)
                        (call-interactively #'aichat-bingai-chat))
         :desc "assistant"
         :nev "a" #'aichat-bingai-assistant
         :desc "replace"
         :nev "i" #'aichat-bingai-replace-or-insert
         :desc "tests"
         :nev "t" #'aichat-bingai-assistant-tests-generator
         )))

;; we recommend using use-package to organize your init.el
(use-package! codeium
  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path

  :init
  ;; use globally
  ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; or on a hook
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  (defun my/set-codemium-capfs ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point))))
  ;; an async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  ;; :defer t ;; lazy loading, if you want
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  )
