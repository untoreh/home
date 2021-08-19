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

(comp-defun evil-duplicate-line ()
            (evil-next-line-1-first-non-blank)
            (evil-visual-char)
            (evil-end-of-line)
            (evil-yank)
            (evil-open-below)
            (evil-paste)
            )

(defun my/force-kill-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (kill-buffer))))

(comp-defun my/collapse-dotted-list (l)
  " Turns a dots filled list into a plain list at the topmost level. "
  (let ((out '()))
    (while (consp l)
      (setq out (nconc out `(,(pop l)))))
    (nconc out `(,l))))

;; (comp-defun my/collapse-dotted-list (l &optional (cc '()))
;;   " Turns a dots filled list into a plain list at the topmost level. "
;;   (if (consp (cdr l))
;;       (apply #'my/collapse-dotted-list
;;              (list (cdr l) (nconc cc `(,(car l)))))
;;     (nconc cc `(,(car l)) `(,(cdr l)))))

;; (comp-defun
;;  my/lists-longer-than (n)
;;  " List top level list items longer than N . Doesn't do any recursion or unwrapping except
;; for dotted pair lists. "
;;  (let ((c 0)
;;        (l (append obarray '()))
;;        (long '()))
;;    (dolist (o l)
;;      (when (and (not (numberp o)) (boundp o))
;;        (let ((val (symbol-value o)))
;;          (when (and (listp val)
;;                     (> (length (my/collapse-dotted-list val)) n))
;;            (nconc long '(o))
;;            (cl-incf c)))))
;;    (list c long)))

;; (cl-defun my/benchmark-gc
;;     (&optional (garbage-amount 16000000)
;;                (tcount 10))
;;   " Run garbage collection TCOUNT times after generating GARBAGE-AMOUNT bytes of garbage"
;;   (let* ((gc-cons-threshold most-positive-fixnum)
;;          (gc (lambda () (gcmh-time (garbage-collect)))))
;;     ;; make sure no garbage is present,
;;     ;; but does not guarantee all garbage is collected
;;     (funcall gc)
;;     (let* ((base (funcall gc))
;;            (times '())
;;            (ratios '())) ;; baseline without garbage
;;       (dotimes (_ tcount)
;;         (make-vector garbage-amount nil) ;; make garbage
;;         (let ((tm (funcall gc))) ;; store gc time
;;           (push tm times)
;;           (push (- (/ tm base) 1) ratios)))
;;       (let* ((avg-ratio (/ (-sum ratios) tcount))
;;              (avg-time (/ (-sum times) tcount))
;;              (pct-ratio (* 100 avg-ratio))
;;              (ms (* 1000 (- avg-time base)))
;;              (base-ms (* 1000 base)))
;;         (message "Collecting %s of garbage takes %.2f%% more time (%.2f ms) than no garbage (%.2f ms)"
;;                  (file-size-human-readable garbage-amount 'si) pct-ratio ms base-ms)))))
;; (cl-defun my/gc-unused ()
;;   " total unused bytes emacs lisp objects stored "
;;   (let* ((gc (garbage-collect))
;;          (unused (mapcar (lambda (x) (* (or (nth 1 x) 0) (or (nth 3 x) 0))) gc)))
;;     (file-size-human-readable (reduce (lambda (x y) (+ x y)) unused) 'si)))
