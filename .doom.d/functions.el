;; -*- lexical-binding: t; -*-
;;

(defun my/script-dir (&optional obj)
  " Returns the path of the function OBJ or the path where my/script-dir is defined "
  (if load-file-name
      (file-name-directory load-file-name)
    (f-dirname (cdr (find-function-library (or obj #'my/script-dir))))))
