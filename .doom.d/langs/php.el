(defun cm-php()
  "comment php error_logs"
  (replace-match "\serror_log(" "\s\/\/error_log\(" ))
(defun decm-php()
  "uncomment php error_logs"
  (replace-match "\s\/\/error_log(" "\serror_log\(" ))

(defun geben--kill-mode ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq (symbol-value 'geben-mode) t)
            (kill-buffer-and-window)))
        (buffer-list)))
