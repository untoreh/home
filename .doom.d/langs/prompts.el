;;; ../../../var/home/fra/.doom.d/langs/prompts.el -*- lexical-binding: t; -*-
;;;
(when (bound-and-true-p blog-tags-list)
  (defun gptel-rewrite-tags ()
    "Select the 4th line in the current buffer and call `#'gptel--suffix-rewrite-and-replaceu on it."
    (interactive)
    (goto-char (point-min))
    (forward-line 3)  ;; Move to the 4th line (0-based index)
    (let ((start (line-beginning-position))
          (end (progn
                 (forward-line 1)
                 (line-end-position)))
          (gptel--rewrite-message "Follow instructions, only reply with the edited text excluding lines starting with '#', don't add any additional char. "
                                  )
          (gptel--system-message gptel--default-system-message)
          (prevctx gptel-context--alist)
          (gptel-context--alist '())
          (this-buf (current-buffer))
          )
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert (concat "This is the list of tags: " (string-join blog-tags-list ",")))
              (gptel-add)


              (with-current-buffer this-buf
                ;; add blog contet as ctx
                (goto-char (point-min))
                (search-forward "+++" nil nil 2)
                (next-line)
                (evil-visual-char)
                (goto-char (point-max))
                (evil-visual-char)
                (gptel-add)

                (goto-char start)
                (push-mark end nil t)
                (gptel--suffix-rewrite-and-replace)
                (gptel-context-remove)
                )
              )
              (my/gptel-clear-context)
            )
        (progn
          (setq gptel-context--alist prevctx))
        ))))
