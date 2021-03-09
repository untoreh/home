

(setq-default exec-path-from-shell-arguments '("-l")) ; disable $PATH change warnings
(setq-default ede-add-project-autoload "generic") ; fixes fail autoload

; disable ivy completion because tab spamming
(setq-default ivy-do-completion-in-region nil)
;; (setq-default completion-at-point-functions '(company-complete-common))
(setq-default completion-at-point-functions nil)

; set default shell
(setq-default explicit-shell-file-name "/bin/sh")
(setq-default exec-path-from-shell-variables '("PATH"))

;; ignore env vars
(cons 'spacemacs-ignored-environment-variables '("EMACS_SOCKET"
                                                  "I3SOCK"
                                                  "SWAYSOCK"
                                                  "MANAGERPID"
                                                  "TMUX"
                                                  "TMUX_PANE"))
;; set env vars
(spacemacs|do-after-display-system-init
 (spacemacs/load-spacemacs-env))
