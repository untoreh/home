
(defun switch-to-next-buffer (&optional window)
  "In WINDOW switch to next buffer.
WINDOW must be a live window and defaults to the selected one.
Return the buffer switched to, nil if no suitable buffer could be
found.

The option `switch-to-prev-buffer-skip' can be used to not switch
to certain buffers, for example, to those already shown in
another window.  Also, if WINDOW's frame has a `buffer-predicate'
parameter, that predicate may inhibit switching to certain
buffers.

This function is called by `next-buffer'."
  (interactive)
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
	 (old-buffer (window-buffer window))
	 (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond
           ((or (functionp switch-to-prev-buffer-skip)
                (memq switch-to-prev-buffer-skip '(t visible 0)))
            switch-to-prev-buffer-skip)
           ((or switch-to-prev-buffer-skip
                (not switch-to-visible-buffer))
            frame)))
	 new-buffer entry killed-buffers skipped)
    (when (window-minibuffer-p window)
      ;; Don't switch in minibuffer window.
      (unless (setq window (minibuffer-selected-window))
	(error "Window %s is a minibuffer window" window)))

    (unless (memq (window-dedicated-p window) '(nil side))
      ;; Don't switch in dedicated window.
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's next buffers first.
      (dolist (buffer next-buffers)
	(when (and (or (buffer-live-p buffer)
		       (not (setq killed-buffers
				  (cons buffer killed-buffers))))
		   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
		   (setq entry (assq buffer (window-prev-buffers window))))
          (if (switch-to-prev-buffer-skip-p skip window buffer)
	      (setq skipped buffer)
	    (setq new-buffer buffer)
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))
      ;; Scan the buffer list of WINDOW's frame next, skipping previous
      ;; buffers entries.  Skip this step for side windows.
      (unless window-side
        (dolist (buffer (buffer-list frame))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
                     ;; Skip buffers whose names start with a space.
                     (not (eq (aref (buffer-name buffer) 0) ?\s))
                     ;; Skip buffers shown in a side window before.
                     (not (buffer-local-value 'window--sides-shown buffer))
                     (not (assq buffer (window-prev-buffers window))))
            (if (switch-to-prev-buffer-skip-p skip window buffer)
	        (setq skipped (or skipped buffer))
              (setq new-buffer buffer)
              (set-window-buffer-start-and-point window new-buffer)
              (throw 'found t)))))
      ;; Scan WINDOW's reverted previous buffers last (must not use
      ;; nreverse here!)
      (dolist (entry (reverse (window-prev-buffers window)))
	(when (and (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
		   (not (eq new-buffer old-buffer))
                   (or (null pred) (funcall pred new-buffer)))
          (if (switch-to-prev-buffer-skip-p skip window new-buffer)
	      (setq skipped (or skipped new-buffer))
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))

      (when skipped
        ;; Show first skipped buffer.
	(setq new-buffer skipped)
	(set-window-buffer-start-and-point window new-buffer)))

    ;; Remove `new-buffer' from and restore WINDOW's next buffers.
    (set-window-next-buffers window (delq new-buffer next-buffers))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))
