;;; langs/gpt.el -*- lexical-binding: t; -*-

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
    (interactive)
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point))))
  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  ;; :defer t ;; lazy loading, if you want
  :config
  ;; (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  ;; (setq codeium-mode-line-enable
  ;;       (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  ;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  ;; (setq codeium-api-enabled
  ;;       (lambda (api)
  ;;         (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  ;; (defun my-codeium/document/text ()
  ;;   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  ;; (defun my-codeium/document/cursor_offset ()
  ;;   (codeium-utf8-byte-length
  ;;    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  ;; (setq codeium/document/text 'my-codeium/document/text)
  ;; (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  )

;; (defcustom gemini-api-key "" "api key for accessing google gemini models")
;; (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)
;;
(use-package! gptel
  :config
  (defcustom deepseek-api-key "" "`gptel' deep seek token")
  (defcustom deepseek-local-api-key "" "`gptel' deep seek token")
  (defcustom openrouter-api-key "" "`gptel' openrouter token")
  (defcustom mistral-api-key "" "`gptel' mistral token")
  (defcustom cohere-api-key "" "`gptel' cohere token")
  (setq openrouter-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key openrouter-api-key
          :models '("google/gemma-2-9b-it:free"
                    "qwen/qwen-2-7b-instruct:free"
                    "microsoft/phi-3-medium-128k-instruct:free"
                    "microsoft/phi-3-mini-128k-instruct:free"
                    "meta-llama/llama-3-8b-instruct:free"
                    "mistralai/mistral-7b-instruct:free"
                    ))
        deepseek-backend (gptel-make-openai "DeepSeekLocal"
                           :host "localhost:8000"
                           :protocol "http"
                           :stream t
                           :key deepseek-local-api-key
                           :models '("deepseek-coder"
                                     "deepseek-chat"
                                     ))
        ;; (gptel-make-openai "Cohere"
        ;;   :host "api.cohere.com"
        ;;   :endpoint "/v1/chat"
        ;;   :stream t
        ;;   :key cohere-api-key
        ;;   :models '("command-r" "command-r-plus"))
        codestral-backend (gptel-make-openai "Mistral"
                            :host "codestral.mistral.ai"
                            :key mistral-api-key
                            :stream t
                            :models '("codestral-latest"))

        gptel-backend deepseek-backend
        gptel-model "deepseek-coder")
  
  (map! (:prefix ("SPC l l" . "gptel")
         :nev "s" #'gptel-send
         :nev "o" #'gptel-menu
         :nev "m" #'gptel
         :nev "a" #'gptel-add
         :nev "k" #'gptel-abort
         :nev "f" #'gptel-add-file
         :nev "r" #'gptel-rewrite-menu
         :nev "p" #'gptel-system-prompt
         :nev "g" #'gptel-beginning-of-response
         :nev "G" #'gptel-end-of-response
         ))
  )
