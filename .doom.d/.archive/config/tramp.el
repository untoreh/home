;; for remote
(require 'tramp)
(setq-default
 ;; tramp-default-host "xnp1"
 ;; tramp-default-user "root"
 tramp-default-host nil
 tramp-default-user nil
 tramp-chunksize 200
 remote-file-name-inhibit-cache nil
 tramp-verbose 1
 vc-ignore-dir-regexp
 (format "%s\\|%s"
         vc-ignore-dir-regexp
         tramp-file-name-regexp
         "/tmp/mnt/*")
 tramp-default-method "ssh"
 tramp-default-method "ssh"
 tramp-use-ssh-controlmaster-options nil
 ;; tramp-ssh-controlmaster-options (concat
 ;;                                  "-o ControlPath=~/.emacs.d/.cache/ssh/.ssh-control-%%r-%%h-%%p "
 ;;                                  "-o ControlMaster=auto -o ControlPersist=yes")
 )

;; login when executing docker shells to source dotfiles
;; (push "-l" (cdr (assoc 'tramp-remote-shell-args (assoc "docker" tramp-methods))))
