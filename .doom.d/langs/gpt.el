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
