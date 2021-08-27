;; -*- lexical-binding: t; -*-
;;

(defun my/script-dir (&optional obj)
  " Returns the path of the function OBJ or the path where my/script-dir is defined "
  (if load-file-name
      (file-name-directory load-file-name)
    (f-dirname (cdr (find-function-library (or obj #'my/script-dir))))))

(defmacro comp-defun (name args &rest body)
  `(progn
     (cl-defun ,name ,args ,@body)
     (native-compile #',name)))

;; (comp-defun evil-duplicate-line ()
;;             (evil-next-line-1-first-non-blank)
;;             (evil-visual-char)
;;             (evil-end-of-line)
;;             (evil-yank)
;;             (evil-open-below)
;;             (evil-paste)
;;             )

(defun my/force-kill-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (kill-buffer))))

