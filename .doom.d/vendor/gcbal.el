;;; wip/gcbal.el -*- lexical-binding: t; -*-
;;;

(defvar gcbal-target-gctime 0.33
  "Desired time for a GC run.
Must be higher than the time spent collecting no garbage.")
(defvar gcbal-target-auto nil
  "Increase target time automatically.
When `gcbal-target-gctime' is too low and `gcbal-target-auto' is t.")

(defvar gcbal-ring-size 5)
(defvar gcbal--adjusted-target-gctime gcbal-target-gctime)
(defvar gcbal-verbose nil)
(defvar gcbal--offsets-ring (make-ring 20)
  "Tracks how well gcbal performed")
(defvar gcbal--base-gctime 0.)

;; TODO: this value should be inferred by sampling the base GC time at different emacs
;; heap sizes and approximate the equation
(defconst gcbal--ref-constant 0.618e-6)
;; System-wise equivalent of the arbitrary `'gcbal--ref-constant
(defvar gcbal--system-constant 0.)
;; These are used for the calculation of `'gcbal--system-constant
(defconst gcbal--ref-gctime 0.445)
(defconst gcbal--ref-mem 1620000)

(defconst gcbal--consed-vec
  [intervals-consed cons-cells-consed vector-cells-consed
                    floats-consed strings-consed symbols-consed string-chars-consed])

;; TODO: weights should be calculated from benchmarking GC on every different atom
(defconst gcbal--weights-table
  (let ((tb (make-hash-table :size 7)))
    (mapc (lambda (k) (puthash k 1. tb))
          '(floats-consed strings-consed symbols-consed string-chars-consed))
    (puthash 'cons-cells-consed 2. tb)
    (puthash 'vector-cells-consed 1.5 tb)
    (puthash 'intervals-consed 1.25 tb)
    tb))
(defconst gcbal--thresholds-ring (make-ring gcbal-ring-size))

(defun gcbal--reset-consed-table ()
  (let ((table (make-hash-table :size 7)))
    (mapc (lambda (k) (puthash k (symbol-value k) table))
          gcbal--consed-vec)
    table))
(defconst gcbal--consed-table (gcbal--reset-consed-table))

(defun gcbal--diff-cons-table ()
  (mapcar (lambda (k)
            (let* ((v (symbol-value k))
                   (prev (gethash k gcbal--consed-table)))
              (puthash k v gcbal--consed-table)
              (* (gethash k gcbal--weights-table)
                 (- v prev))))
          gcbal--consed-vec))

(defun gcbal--emacs-memory-usage ()
  " TODO: this is most likely the biggest bottle-neck "
  (alist-get 'rss (process-attributes (emacs-pid))))

(defun gcbal--ma-ring (ring)
  (let ((c 0)
        (s 0)
        (size (ring-size ring)))
    (while (< c size)
      (setq s (+ s (ring-ref ring c)))
      (cl-incf c))
    (/ s size)))

(defun gcbal--adjust-threshold ()
  (let* ((consed (-sum (gcbal--diff-cons-table)))
         (min-gctime (* gcbal--unit-gctime
                        (gcbal--emacs-memory-usage)))
         (target-offset (- gcbal--adjusted-target-gctime min-gctime))
         (below-base (< target-offset 0.))
         (last-gctime (- gc-elapsed gcbal--elapsed))
         (last-offset (max 0. (- last-gctime min-gctime)))
         (threshold (if (and (not (equal 0. last-offset))
                             (not below-base))
                        (truncate (/ (* target-offset consed) last-offset))
                      ;; If we can't tune gc because outside min and max,
                      ;; double the threshold, because in either case it is
                      ;; going to increase
                      (* 2 gc-cons-threshold))))
    (when below-base
      (message "target time for GC cannot be reached
because %fs falls below the current minimum time of %fs"
               gcbal-target-gctime min-gctime)
      (when gcbal-target-auto
        (cl-incf gcbal--adjusted-target-gctime min-gctime)))

    (ring-insert gcbal--thresholds-ring  threshold)
    (setq gc-cons-threshold (gcbal--ma-ring gcbal--thresholds-ring)
          gcbal--elapsed gc-elapsed)

    (when gcbal-verbose
      (ring-insert gcbal--offsets-ring last-offset)
      (message "gcbal: min %f, trg: %f, last: %f, cns: %f, rat: %f, accu: %f"
               min-gctime target-offset last-offset
               consed (/ target-offset last-offset)
               (gcbal--ma-ring gcbal--offsets-ring)
               )
      )))

(cl-defun gcbal--calc-base-gctime (&optional (times 10))
  (garbage-collect)
  (/ (caddr (benchmark-call #'garbage-collect times))
     (float times)))

(defun gcbal--reset-threshold ()
  (let ((threshold 4096))
    (gcbal--adjust-system-constant)
    (setq
     gcbal--unit-gctime (* gcbal--system-constant gcbal--base-gctime)
     gcbal--adjusted-target-gctime gcbal-target-gctime
     gcbal--elapsed gc-elapsed
     gc-cons-threshold threshold)
    (dotimes (_ gcbal-ring-size)
      (ring-insert gcbal--thresholds-ring threshold))))

(defun gcbal--adjust-system-constant (&optional reset)
  " TODO: needs macros "
  (require 'pcache)
  (let ((repo (pcache-repository 'gcbal)))
    (setq
     gcbal--base-gctime
     (if (and (not reset)
              (pcache-has repo 'gcbal--base-gctime))
         (pcache-get repo 'gcbal--base-gctime)
       (gcbal--calc-base-gctime))

     gcbal--system-constant
     (if (and (not reset)
              (pcache-has repo 'gcbal--system-constant))
         (pcache-get repo 'gcbal--system-constant)
       (/ (* gcbal--ref-constant
             (/ (gcbal--emacs-memory-usage)
                gcbal--base-gctime))
          (/ gcbal--ref-mem gcbal--ref-gctime))
       ))
    (pcache-put repo 'gcbal--system-constant gcbal--system-constant)
    (pcache-put repo 'gcbal--base-gctime gcbal--base-gctime)))

(defvar gcbal--gcfun (symbol-function #'garbage-collect))
;; (let ((gc-data (garbage-collect)))
;;     (mapc (lambda (s) (let ((s (cddr s)))
;;                      (dotimes (i (length s)) (setf (nth i s) 0)))) gc-data)
;;     (lambda () (cdr gc-data)))
(defconst gcbal--stub
  (lambda () '((conses 16 0 0)
          (symbols 48 0 0)
          (strings 32 0 0)
          (string-bytes 1 0)
          (vectors 16 0)
          (vector-slots 8 0 0)
          (floats 8 0 0)
          (intervals 56 0 0)
          (buffers 0 0))))


;;;###autoload
(define-minor-mode gcbal-mode
  "Minor mode to tweak Garbage Collection strategy."
  :lighter " GCBAL"
  :global t
  (if gcbal-mode
      (progn
        (when (boundp #'gcmh-mode)
          (gcmh-mode -1))

        (gcbal--reset-threshold)
        (gcbal--reset-consed-table)

        (fset #'garbage-collect gcbal--stub)
        (add-hook 'post-gc-hook #'gcbal--adjust-threshold)
        ;; (fset #'garbage-collect #'gcbal--garbage-collect)
        )
    (fset #'garbage-collect gcbal--gcfun)
    (remove-hook 'post-gc-hook #'gcbal--adjust-threshold)
    (setq gc-cons-threshold 800000)))

(provide 'gcbal)
