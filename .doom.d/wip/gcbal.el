;;; wip/gcbal.el -*- lexical-binding: t; -*-

(require 'benchmark)

(defcustom gcbal-baseline-factor 1
  " The weight of the ratio between memory and gc time.
Define the ratio between how long it takes for GC to run
in respect to the size of the emacs heap. ")

(defconst gcbal--consed-list
  '(intervals-consed cons-cells-consed vector-cells-consed
                     floats-consed strings-consed symbols-consed string-chars-consed))
(defvar gcbal--weights-table
  (let ((tb (make-hash-table :size 7)))
    (mapc (lambda (k) (puthash k 1. tb))
          '(floats-consed strings-consed symbols-consed string-chars-consed))
    (puthash 'cons-cells-consed 2. tb)
    (puthash 'vector-cells-consed 1.5 tb)
    (puthash 'intervals-consed 1.25 tb)
    tb))

(defvar gcbal--idle-delay 1)
(defvar gcbal--target-gc-time 0.3)

(defun gcbal-emacs-memory-usage ()
  (string-to-number
   (shell-command-to-string
    (format "ps -o rss --no-headers %d" (emacs-pid)))))

(defun gcbal--update-consed-table ()
  (when (or (not (boundp 'gcbal--consed-table))
            (not (hash-table-p gcbal--consed-table)))
    (setq gcbal--consed-table (make-hash-table :size 7)))
  (mapc (lambda (k) (puthash k (symbol-value k) gcbal--consed-table))
        gcbal--consed-list))

(defun gcbal--diff-cons-table ()
  (mapcar (lambda (k)
            (let* ((v (symbol-value k))
                   (prev (gethash k gcbal--consed-table)))
              (puthash k v gcbal--consed-table)
              (* (gethash k gcbal--weights-table)
                 (- v prev))))
          gcbal--consed-list))

(defun gcbal--compute-delay ()
  (let ((garb (-sum (gcbal--diff-cons-table))))
    ()
    ))

(defun gcbal-set-baseline ()
  (garbage-collect)
  (setq gcbal--base-time (* 1000 (caddr (benchmark-call #'garbage-collect)))
        gcbal--base-mem (gcbal-emacs-memory-usage)
        gcbal--mem-gc-ratio (/ gcbal--base-mem gcbal--base-time)))
