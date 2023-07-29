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
