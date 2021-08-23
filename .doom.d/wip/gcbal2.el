;;; wip/gcbal.el -*- lexical-binding: t; -*-
;;;
(require 'benchmark)

(defvar gcbal-target-gctime 0.33
  "Desired time for a GC run. Must be higher than the time spent collecting no garbage.")
(defvar gcbal-verbose nil)
(defvar gcbal-target-idle-gctime 0.11)
(defvar gcbal--last-offset 0.)
(defvar gcbal--unit-gctime 0.)
(defvar gcbal--offset-gctime 0.)

(defconst gcbal--consed-list
  '(intervals-consed cons-cells-consed vector-cells-consed
                     floats-consed strings-consed symbols-consed string-chars-consed))
(defconst gcbal--consed-table (make-hash-table :size 7))
(defconst gcbal--weights-table
  (let ((tb (make-hash-table :size 7)))
    (mapc (lambda (k) (puthash k 1. tb))
          '(floats-consed strings-consed symbols-consed string-chars-consed))
    (puthash 'cons-cells-consed 2. tb)
    (puthash 'vector-cells-consed 1.5 tb)
    (puthash 'intervals-consed 1.25 tb)
    tb))

(defun gcbal--update-consed-table ()
  (mapc (lambda (k) (puthash k (symbol-value k) gcbal--consed-table))
        gcbal--consed-list))

(defun gcbal--diff-cons-table ()
  (mapcar (lambda (k)
            (let* ((v (symbol-value k))
                   (prev (gethash k gcbal--consed-table)))
              (puthash k v gcbal--consed-table)
              (message "wow")
              (* (gethash k gcbal--weights-table)
                 (- v prev))))
          gcbal--consed-list))

(defun gcbal--emacs-memory-usage ()
  (string-to-number
   (shell-command-to-string
    (format "ps -o rss --no-headers %d" (emacs-pid)))))

(defun gcbal--log-start ()
  (setq gcbal--gc-start (current-time)))

(defun gcbal--adjust-threshold ()
  (let* ((time (current-time)) ;; this needs to be first
         (consed (-sum (gcbal--diff-cons-table)))
         (base-gctime (* gcbal--unit-gctime (gcbal--emacs-memory-usage)))
         (target-offset (- gcbal-target-gctime base-gctime))
         (last-offset (max 0. (- (float-time (time-since gcbal--gc-start))
                                 base-gctime
                                 ;; remove let time from gc time calc
                                 (float-time (time-since time))))))
    (when (not (equal 0. last-offset))
      (setq gc-cons-threshold
              (truncate
               (/ (* target-offset consed) last-offset))))
   (setq gcbal--last-offset last-offset)
   (when gcbal-verbose
     (message "gcbal: base %f, trg: %f, last: %f, cns: %f, rat: %f"
              base-gctime target-offset last-offset consed (/ target-offset last-offset)))))

(defun gcbal--reset-threshold ()
  (setq
   gcbal--unit-gctime
   (progn
     (garbage-collect)
     (/ (caddr (benchmark-call #'garbage-collect 1))
        (gcbal--emacs-memory-usage)))
   gc-cons-threshold 4096)
  (gcbal--update-consed-table))

;;;###autoload
(define-minor-mode gcbal-mode
  "Minor mode to tweak Garbage Collection strategy."
  :lighter " GCBAL"
  :global t
  (if gcbal-mode
      (progn
        (require 'benchmark)
        (when (boundp #'gcmh-mode)
          (gcmh-mode -1))
        (gcbal--reset-threshold)
        (advice-add #'garbage-collect :before #'gcbal--log-start)
        (advice-add #'garbage-collect :after #'gcbal--adjust-threshold)
        (when (fboundp #'native-compile-async)
          (native-compile-async (cdr (find-function-library 'gcbal-mode)))))
    (advice-remove #'garbage-collect #'gcbal--log-start)
    (advice-remove #'garbage-collect #'gcbal--adjust-threshold)))

(provide 'gcbal)
