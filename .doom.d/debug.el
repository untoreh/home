;;; ../../../var/home/fra/.doom.d/debug.el -*- lexical-binding: t; -*-

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

(comp-defun my/collapse-dotted-list (l &optional (cc '()))
            " Turns a dots filled list into a plain list at the topmost level. "
            (if (consp (cdr l))
                (apply #'my/collapse-dotted-list
                       (list (cdr l) (nconc cc `(,(car l)))))
              (nconc cc `(,(car l)) `(,(cdr l)))))

(comp-defun
 my/lists-longer-than (n)
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
    (file-size-human-readable (reduce (lambda (x y) (+ x y)) unused) 'si)))
