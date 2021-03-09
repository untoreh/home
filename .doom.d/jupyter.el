;;; ../../../var/home/fra/.doom.d/jupyter.el -*- lexical-binding: t; -*-

;; FIXME: Patched jupyter-org function to ensure that the output inserted in a
;; src block result doesn't hang emacs (because of too long lines),
;; also relay color codes as they are received by the server
(after! ob-jupyter
  (defun jupyter-org--do-insert-result (req result)
    (if (and (stringp result) (> (length result) (window-total-width)))
        (let* ((max-width (- (window-total-width) 2))
               (half-width (/ max-width 2))
               (result-split (split-string result "[\n]"))
               (result-len (length result)))
          (setq
           result
           (concam
            ;; (format "Result was truncated, because it was too long! (%d)\n"
            ;;         result-len)
            (mapconcat
             #'identity
             (mapcar
              #'(lambda (str)
                  (if (> (length str) max-width)
                      (concat (substring str 0 half-width) " ... "
                              (substring str (- half-width))) str))
              result-split) "\n")))))
    (org-with-point-at
        (jupyter-org-request-marker req)
      (let ((res-begin (org-babel-where-is-src-block-result 'insert)))
        (goto-char res-begin)
        (let* ((indent (current-indentation))
               (context (jupyter-org--normalized-insertion-context))
               (pos (jupyter-org--append-stream-result-p context result)))
          (cond
           (pos
            (goto-char pos)
            (jupyter-org-indent-inserted-region
             indent
             (jupyter-org--append-stream-result result)))
           (t
            (forward-line 1)
            (unless (bolp) (insert "\n"))
            (jupyter-org--prepare-append-result context)
            (jupyter-org-indent-inserted-region
             indent
             (jupyter-org--insert-result req context result))))
          (when (jupyter-org--stream-result-p result)
            (let ((end (point-marker)))
              (unwind-protect
                  (jupyter-org--handle-control-codes
                   (if pos
                       (save-excursion
                         (goto-char pos)
                         ;; Go back one line to account for an edge case
                         ;; where a control code is at the end of a line.
                         (line-beginning-position 0))
                     res-begin)
                   end)
                (set-marker end nil)))
            (jupyter-org--mark-stream-result-newline result))
          (ansi-color-apply-on-region (plist-get (car (cdr context)) ':end) (point)))))))
