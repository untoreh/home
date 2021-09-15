;;; calibredb-extras.el -*- lexical-binding: t; -*-

(require 'calibredb)
(require 's)

(defun calibredb-args (&key quotes &rest args)
  (apply #'format `(,(s-repeat (length args) (if quotes "'%s' " "%s "))
                    ,@args)))


(defun calibredb-add-column (label name datatype)
  (calibredb-command
   :command "add_custom_column"
   :library (calibredb-arg-library 'local)
   :input (calibredb-args label name datatype)))

(defun calibredb-arg-library (&optional use-local)
  (format "--library-path %s"
          (if-let (url (and (null use-local)
                            (calibredb-check-server)))
              url
            (calibredb-root-dir-quote))))

(defun calibredb-add-read-column ()
  (let* ((columns (calibredb-command
                   :command "custom_columns"
                   :library (calibredb-arg-library 'local)))
         (cols (split-string columns "\n")))
    (unless (stringp (catch 'match
                       (mapc (lambda (x) (and (string-prefix-p "read_status" x)
                                         (throw 'match x))) cols)))
      (calibredb-add-column "read_status" "Read Status" "bool"))))

(defun calibredb-check-server ()
  (and (bound-and-true-p calibredb-server-host)
       (bound-and-true-p calibredb-server-port)
       (processp (ignore-errors (open-network-stream "" nil calibredb-server-host calibredb-server-port)))
       (concat "http://" calibredb-server-host ":" calibredb-server-port)))

(defun calibredb-set-custom (column id value)
  (calibredb-command
   :library (calibredb-arg-library 'local)
   :command "set_custom"
   :input (calibredb-args :key nil column id value)))

(defun calibredb-candidate-key (key &optional candidate)
  (car (alist-get key
                  (caar (or candidate
                            (calibredb-find-candidate-at-point))))))

(defun calibredb-set-read (&optional candidate)
  (calibredb-set-custom "read_status"
                        (calibredb-candidate-key :id)
                        "true"))

(defun calibredb-set-custom-at-point (col val)
  (interactive "sColumn: \nsValue: ")
  (calibredb-set-custom col
                        (calibredb-candidate-key :id)
                        val))

(cl-defun calibredb-set-read-at-point (&optional (status t))
  (interactive)
  (let ((status (if status "true" "false")))
    (calibredb-set-custom "read_status"
                          (calibredb-candidate-key :id)
                          status)
    (message "Read status for book %s updated to %s"
             (calibredb-candidate-key :book-name)
             status)))

;; calibredb
(map! :desc "calibredb" :leader :nv "o l" #'calibredb)

(map! :map calibredb-search-mode-map
      :ne "C" #'calibredb-set-custom-at-point
      :ne "r" (cmd! (calibredb-set-read-at-point))
      :ne "R" (cmd! (calibredb-set-read-at-point nil)))

(map! :map calibredb-show-mode-map
      ;; standard
      :ne "?" #'calibredb-entry-dispatch
      :ne "o" #'calibredb-find-file
      :ne "O" #'calibredb-find-file-other-frame
      :ne "V" #'calibredb-open-file-with-default-tool
      :ne "s" #'calibredb-set-metadata-dispatch
      :ne "e" #'calibredb-export-dispatch
      :ne "q" #'calibredb-entry-quit
      :ne "." #'calibredb-open-dired
      :ne [tab] #'calibredb-toggle-view-at-point
      :ne "M-t" #'calibredb-set-metadata--tags
      :ne "M-a" #'calibredb-set-metadata--author_sort
      :ne "M-A" #'calibredb-set-metadata--authors
      :ne "M-T" #'calibredb-set-metadata--title
      :ne "M-c" #'calibredb-set-metadata--comments)
(map! :map calibredb-search-mode-map
      :ne [mouse-3] #'calibredb-search-mouse
      :ne "RET" #'calibredb-find-file
      :ne "?" #'calibredb-dispatch
      :ne "a" #'calibredb-add
      :ne "A" #'calibredb-add-dir
      :ne "c" #'calibredb-clone
      :ne "d" #'calibredb-remove
      :ne "D" #'calibredb-remove-marked-items
      :ne "j" #'calibredb-next-entry
      :ne "k" #'calibredb-previous-entry
      :ne "l" #'calibredb-virtual-library-list
      :ne "L" #'calibredb-library-list
      :ne "n" #'calibredb-virtual-library-next
      :ne "N" #'calibredb-library-next
      :ne "p" #'calibredb-virtual-library-previous
      :ne "P" #'calibredb-library-previous
      :ne "s" #'calibredb-set-metadata-dispatch
      :ne "S" #'calibredb-switch-library
      :ne "o" #'calibredb-find-file
      :ne "O" #'calibredb-find-file-other-frame
      :ne "v" #'calibredb-view
      :ne "V" #'calibredb-open-file-with-default-tool
      :ne "." #'calibredb-open-dired
      :ne "b" #'calibredb-catalog-bib-dispatch
      :ne "e" #'calibredb-export-dispatch
      :ne "r" #'calibredb-search-refresh-and-clear-filter
      :ne "R" #'calibredb-search-clear-filter
      :ne "q" #'calibredb-search-quit
      :ne "m" #'calibredb-mark-and-forward
      :ne "f" #'calibredb-toggle-favorite-at-point
      :ne "x" #'calibredb-toggle-archive-at-point
      :ne "h" #'calibredb-toggle-highlight-at-point
      :ne "u" #'calibredb-unmark-and-forward
      :ne "i" #'calibredb-edit-annotation
      :ne "DEL" #'calibredb-unmark-and-backward
      :ne [backtab] #'calibredb-toggle-view
      :ne [tab] #'calibredb-toggle-view-at-point
      :ne "M-n" #'calibredb-show-next-entry
      :ne "M-p" #'calibredb-show-previous-entry
      :ne "/" #'calibredb-search-live-filter
      :ne "M-t" #'calibredb-set-metadata--tags
      :ne "M-a" #'calibredb-set-metadata--author_sort
      :ne "M-A" #'calibredb-set-metadata--authors
      :ne "M-T" #'calibredb-set-metadata--title
      :ne "M-c" #'calibredb-set-metadata--comments)


;; config
(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Documents/books"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-server-host "http://localhost"
        calibredb-server-port "8099"
        calibredb-library-alist '(("~/Documents/books")))
  (calibredb-add-read-column))
