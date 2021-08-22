;;; wip/gcbal.el -*- lexical-binding: t; -*-

(require 'benchmark)

(defvar gcbal-target-gctime 0.33)
;; (defvar gcbal-target-idle-gctime 0.11)
(defvar gcbal--last-gctime 0.)
(defvar gcbal--max-cons-threshold 1073741824)
(defvar gcbal--min-cons-threshold 800000)
(defvar gcbal--base-gctime 0.)
(defvar gcbal--offset-gctime 0.)

(comp-defun gcbal--log-start ()
  (setq gcbal--gc-start (current-time)))

(comp-defun
 gcbal--adjust-threshold ()
 (let* ((gctime (max 0. (- (float-time (time-since gcbal--gc-start))
                   gcbal--base-gctime))))
   (setq gc-cons-threshold
         (max
          gcbal--min-cons-threshold
          (min
           gcbal--max-cons-threshold
           (truncate
            (* gc-cons-threshold
               (if (not (equal 0. gctime))
                   (/ gcbal--offset-gctime gctime)
                 1.))))))
   (setq gcbal--last-gctime gctime)
   ;; (message "gc time is %f, thresh: %d" gctime gc-cons-threshold)
   ))

(defun gcbal--reset-threshold ()
  (setq
   gcbal--base-gctime
   (progn
     (garbage-collect)
     (caddr (benchmark-call #'garbage-collect 1)))
   gc-cons-threshold 33554432)
  (setq gcbal--offset-gctime
        (max 0 (- gcbal-target-gctime gcbal--base-gc-time))))

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
        (advice-add #'garbage-collect :before #'gcbal--log-start)
        (advice-add #'garbage-collect :after #'gcbal--adjust-threshold))
    (advice-remove #'garbage-collect #'gcbal--log-start)
    (advice-remove #'garbage-collect #'gcbal--adjust-threshold)))
