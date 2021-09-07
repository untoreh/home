;;; ../../../var/home/fra/.doom.d/debug.el -*- lexical-binding: t; -*-

(use-package! caliper)

(use-package! etrace
  :after elp)

(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun my/call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (let* ((log     nil)
         (logger (lambda (&rest hooks)
                   (setq log (append log hooks nil)))))
    (my/with-advice
     ((#'run-hooks :before logger))
     (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))

(cl-defun my/collapse-dotted-list (l &optional (cc '()))
            " Turns a dots filled list into a plain list at the topmost level. "
            (if (consp (cdr l))
                (apply #'my/collapse-dotted-list
                       (list (cdr l) (nconc cc `(,(car l)))))
              (nconc cc `(,(car l)) `(,(cdr l)))))

(defun my/lists-longer-than (n)
 " List top level list items longer than N . Doesn't do any recursion or unwrapping except
for dotted pair lists. "
 (let ((c 0)
       (l (append obarray '()))
       (long '()))
   (dolist (o l)
     (when (and (not (numberp o)) (boundp o))
       (let ((val (symbol-value o)))
         (when (and (listp val)
                    (> (length (my/collapse-dotted-list val)) n))
           (setq long (nconc long `(,o)))
           (cl-incf c)))))
   (list c long)))

(cl-defun my/benchmark-gc
    (&optional (garbage-amount 16000000)
               (tcount 10)
               (struct 'list))
  " Run garbage collection TCOUNT times after generating GARBAGE-AMOUNT bytes of garbage"
  (interactive)
  (require 'gcmh)
  (let* ((gc-cons-threshold most-positive-fixnum)
         (gc (lambda () (gcmh-time (garbage-collect))))
         (alloc (pcase struct
                  ('list #'make-list)
                  ('vector  #'make-vector)
                  ('string (lambda (size _) (make-string size 0))))))
    ;; make sure no garbage is present,
    ;; but does not guarantee all garbage is collected
    (funcall gc)
    (let* ((base (funcall gc))
           (times '())
           (ratios '())) ;; baseline without garbage
      (dotimes (_ tcount)
        (funcall alloc garbage-amount "") ;; make garbage
        (let ((tm (funcall gc))) ;; store gc time
          (push tm times)
          (push (- (/ tm base) 1) ratios)))
      (let* ((avg-ratio (/ (-sum ratios) tcount))
             (avg-time (/ (-sum times) tcount))
             (pct-ratio (* 100 avg-ratio))
             (ms (* 1000 (- avg-time base)))
             (base-ms (* 1000 base)))
        (message "Collecting %s of garbage takes %.2f%% more time (%.2f ms) than no garbage (%.2f ms)"
                 (file-size-human-readable garbage-amount 'si) pct-ratio ms base-ms)))))
                                        ;
(cl-defun my/gc-unused ()
  " total unused bytes emacs lisp objects stored "
  (let* ((gc (garbage-collect))
         (unused (mapcar (lambda (x) (* (or (nth 1 x) 0) (or (nth 3 x) 0))) gc)))
    (file-size-human-readable (cl-reduce (lambda (x y) (+ x y)) unused) 'si)))

(defun my/backtrace-nth-func (n)
  " If n is 0, it should return the function name symbol. "
  (cadr (nth (+ 3 n) (backtrace-frames))))

(cl-defun my/genlist
    (&optional (n 4096)
               (fgen (lambda () 0)))
  " Generates a list of length N with values returned by FGEN"
  (let ((l (list)))
    (dotimes (_ n) (push (funcall fgen) l))
    l))

(cl-defun my/genvector
    (&optional (n 4096)
               (fgen (lambda () 0)))
  " Generates a vector of length N with values returned by FGEN"
  (let ((vec (make-vector n nil)))
    (dotimes (i n)
      (setf (seq-elt vec i) (funcall fgen)))
  vec
  ))

(cl-defun my/garbage-list (&optional (depth-1 64) (depth-2 32) (raw nil))
  " Generate a list of length DEPTH-1 of lists of length DEPTH-2 "
  (let* ((max-lisp-eval-depth most-positive-fixnum)
         (max-specpdl-size most-positive-fixnum)
         (fgen-2 (lambda () (cl-copy-list (my/genlist depth-2))))
         (fgen-1 (lambda () (cl-copy-list (my/genlist depth-2 fgen-2))))
         (l (my/genlist depth-1 fgen-1))
         (size (caliper-object-size l))
         )
    (if raw
        size
     (file-size-human-readable size) )))

(cl-defun my/garbage-vect (&optional (depth-1 64) (depth-2 44) (raw nil))
  " Generate a vector of length DEPTH-1 of vectors of length DEPTH-2 "
  (let* ((max-lisp-eval-depth most-positive-fixnum)
         (max-specpdl-size most-positive-fixnum)
         (fgen-2 (lambda () (cl-copy-seq (my/genvector depth-2))))
         (fgen-1 (lambda () (cl-copy-seq (my/genvector depth-2 fgen-2))))
         (l (my/genvector depth-1 fgen-1))
         (size (caliper-object-size l)))
    (if raw
        size
      (file-size-human-readable size))))

(cl-defun my/gc-bench(fn &optional (times 3))
  (garbage-collect)
  (first (last (benchmark-call
   (lambda () (funcall fn)
     (garbage-collect)
     ) times))))

;; (let* ((d 1000)
;;       (n 100)
;;       )
;;   (comp-defun f1 ()
;;               (let ((l (my/genlist d)))
;;                 (dotimes (_ n) (cl-copy-seq l))
;;                 ))

;;   (comp-defun f2 ()
;;               (let ((v (my/genvector d)))
;;                 (dotimes (_ n) (cl-copy-seq v))
;;                 ))
;;   (/ (my/gc-bench 'f1)
;;      (my/gc-bench 'f2)
;;      )
;;   )
