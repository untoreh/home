;;; langs/julia-proto.el -*- lexical-binding: t; -*-

(defconst julia--regexp-struct (rxt-pcre-to-elisp  "^struct\s"))
(defconst julia--regexp-proto-struct (rxt-pcre-to-elisp  "^@proto struct\s"))
(defconst julia--regexp-proto-module (rxt-pcre-to-elisp  "^using ProtoStructs\n"))
(defun julia--proto-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward julia--regexp-proto-module nil t)
    ))
(defun julia--protify (&optional flag)
  (interactive)
  (if (equal major-mode #'julia-mode)
      (save-excursion
        (goto-char (point-min))
        (if flag
            (progn
              (insert "using ProtoStructs\n")
              (while (re-search-forward  julia--regexp-struct nil t)
                (replace-match "@proto struct ")))
          (progn
            (while (re-search-forward  julia--regexp-proto-module nil t)
              (replace-match ""))
            (while (re-search-forward  julia--regexp-proto-struct nil t)
              (replace-match "struct "))))
        (save-buffer))
    (warn "Current buffer is not a julia buffer.")))

(defun julia-toggle-proto-structs ()
  (if (julia--proto-p)
      (julia--protify)
    (julia--protify t)
    )
  )

(defun julia-deprotify-structs ()
  (if (equal major-mode #'julia-mode)
      (save-excursion
        (goto-char (point-min))
        (insert "using ProtoStructs\n")
        (replace-regexp  julia-regexp-struct "@proto struct ")
        )
    (warn "Current buffer is not a julia buffer."))
  )
