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

(defun my/float-to-floor-precision (num prec)
  " TODO: is this type stable? "
  (string-to-number
   (format (concat "%." (number-to-string prec) "f")
           num)))

;; recursive version of my/first-common-prefix
;; (cl-defun my/first-common-prefix (candidates &optional (min-length 2))
;;   (let ((common (try-completion "" candidates)))
;;     (if (not (eq "" common))
;;         common
;;       (my/first-common-prefix (butlast candidates) min-length))))

(cl-defun my/first-common-prefix (candidates)
  (let ((can candidates)
        (match ""))
  (cl-dotimes (_ (- (length candidates) 1))
    (setq match (try-completion "" can))
    (when (not (eq match ""))
      (cl-return match))
    (set can (butlast can)))
  (if (not (eq "" match))
      match
    nil)))

(defun my/company-update-first-common (&rest _)
  (setq company-common (my/first-common-prefix company-candidates)))

(defun my/ensure-symlink (trg name)
  (let ((orig name)
        (trg (file-truename trg))
        (name (file-truename name)))
  (cond
    ((not (file-exists-p name))
     (progn
       (and (file-symlink-p name) (delete-file name))
       (make-directory (file-name-directory name) t)
       (make-symbolic-link trg name)))
    ((and (file-symlink-p name)
          (not (equal (file-truename trg)
                      (file-truename name))))
     (progn
       (delete-file name)
       (make-symbolic-link trg name)))
    ((and (file-exists-p name)
          (not (file-symlink-p orig))) (error "%s is not a symlink" orig))
    )))

(defun my/concat-path (&rest parts)
  (-reduce (lambda (a b) (expand-file-name b a)) parts))

(provide 'functions)
