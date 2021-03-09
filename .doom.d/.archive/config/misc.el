;; files
;; (add-to-list 'recentf-keep '("\.emacs\.d\/\.cache\/scratches\/.*"))
(setq-default scratches-dir (concat spacemacs-cache-directory "scratches"))
(make-directory scratches-dir t)
(defun spacemacs/new-empty-file ()
  (interactive)
  (spacemacs/new-empty-buffer)
  (setq default-directory scratches-dir)
  (set-visited-file-name
   (make-auto-save-file-name)))
(spacemacs/set-leader-keys "fN" 'spacemacs/new-empty-file)

;; Makes possible to sudo-edit files
;; From Damien Cassou
;; https://github.com/renard/dired-toggle-sudo/issues/1
(defun sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

;; cleanup
; (setq-default message-log-max nil)
