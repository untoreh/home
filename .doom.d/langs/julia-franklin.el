;;; langs/julia-franklin.el -*- lexical-binding: t; -*-

(set-file-template! ".*/blog/posts/.+\\.md$" :trigger "auto_blog_post" :project t)

(require 'aio)
(aio-defun julia-franklin ()
  (interactive)
  "Start the franklin live server in the current default-dir"
  (julia-repl-cmd
   (f-read-text
    (concat
     (file-name-as-directory
      (my/script-dir #'julia-franklin)) "franklin.jl"))
   1) ;; wait at startup
  (vterm-send-return))

(aio-defun julia-franklin-maybe-stop ()
  (catch 'stop
    (julia-repl-switch t nil)
    (let ((buf (current-buffer)))
      (while t
        (unwind-protect
            (progn
              (switch-to-buffer (julia-repl-live-buffer))
              (vterm-maybe-reset-cursor
               ".*Use Pkg.activate() to go back"
               (vterm-send-C-c)
               (throw 'stop t))
              (vterm--maybe-reset-cursor
               (concat julia-prompt-regexp "$")
               (throw 'stop t)))
          (switch-to-buffer buf)
          (aio-await (aio-sleep 0.2))
          (switch-to-buffer buf)
          )))))

(defun julia-franklin-stop ()
  (interactive)
  (julia-repl-cmd "try Base.throwto(frank_task, InterruptException()) catch end")
  (julia-franklin-maybe-stop))

(after! f
  (cl-defun julia-franklin-sync-blog (&optional (src "/tmp/__site")
                                                &optional (trg (concat
                                                                (getenv "HOME")
                                                                "/dev/blog/__site.bak")))
    " Creates a symblink in target directory to src directory name if doesn't exist and syncs
the SRC folder to the TRG folder"
    (interactive)
    (let ((src-sym (concat (file-name-directory trg) (f-base src))))
      (if (or (not (file-directory-p src))
              (directory-empty-p src))
          (start-process "sync-blog-tmp" "*rsync*" "rsync"
                         "-a" (file-name-as-directory trg)
                         (file-name-as-directory src))
        (when (not (file-directory-p src))
          (error "%s exists but it's not a directory" src)))
      (if (not (and (file-symlink-p src-sym)
                    (equal (file-truename src-sym) src)))
          (if (file-exists-p src-sym)
              (error "%s exists and is not a symlink" src-sym)
            (make-symbolic-link src src-sym)))
      ;; sync from tmp to disk
      (if (file-directory-p trg)
          (start-process "sync-blog-disk" "*rsync*"
                         "rsync" "-a" (file-name-as-directory src)
                         (file-name-as-directory trg))
        (error "%s is not a valid target directory, wrong project?" trg)))))

(defun julia-franklin-publish ()
  (interactive)
  (julia-franklin-stop)
  (julia-repl-cmd "pubup()"))

(defun julia-franklin-serve()
  (interactive)
  (julia-franklin-stop)
  (julia-repl-cmd "frank_task = @task serve(prerender=true, is_final_pass=true); schedule(frank_task)")
  (vterm-send-return))

;;; add tag completion to blog posts
(after! corfu
  (defconst
    blog-tags-list
    (with-temp-buffer
      (insert-file-contents
       (my/concat-path doom-private-dir
                       "langs"
                       "blog_tags.txt"))
      (split-string (buffer-string)))
    "The list of tags for the blog tags backend.")
  (defun blog-tags-capf ()
    "Completes based on a list of tags defined in `company-blog-tags'."
    (interactive)
    (let* ((start (line-beginning-position))
           (beg (buffer-substring-no-properties start (min (buffer-size) (+ start 6))))
           )
      (when (equal beg "tags =")
        (let* ((pos (point))
               (word (word-at-point))
               (beg (- pos (length word)))
               (end pos)
               )
          (list beg pos blog-tags-list . nil)
          ))
      )
    )

  (defvar my/prev-capfs nil "Stores previous company backends in buffer.")
  (define-minor-mode blog-tags-mode
    "Minor mode for blog tags completion."
    :lighter "blog-tags-capf"
    :global nil
    (if blog-tags-mode
        (progn
          (setq-local my/prev-capfs completion-at-point-functions)
          (setq-local completion-at-point-functions '(blog-tags-capf))
          )
      ))

  (map! :mode gfm-mode
        :localleader
        :desc "Toggle completion of blog tags."
        :nv "t" (cmd! (blog-tags-mode (if blog-tags-mode -1 1)))
        )
  )
