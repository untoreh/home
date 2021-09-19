;;; calibredb-extras.el -*- lexical-binding: t; -*-

(require 'calibredb)
(require 's)
(require 'request)

(defun calibredb-args (&key quotes &rest args)
  (apply #'format `(,(s-repeat (length args) (if quotes "'%s' " "%s "))
                    ,@args)))

(defun calibredb-check-server ()
  (and (bound-and-true-p calibredb-server-host)
       (bound-and-true-p calibredb-server-port)
       (processp (ignore-errors (open-network-stream "" nil calibredb-server-host calibredb-server-port)))
       (concat "http://" calibredb-server-host ":" calibredb-server-port)))

;; CALIBREDB SUPPORT FOR CUSTOM COLUMNS
(defun calibredb-candidate-key (key &optional candidate)
  (calibredb-getattr
   (or candidate
       (first (calibredb-find-candidate-at-point)))
   :id))

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


(defun calibredb-set-custom (column id value)
  (calibredb-command
   :library (calibredb-arg-library 'local)
   :command "set_custom"
   :input (calibredb-args :key nil column id value)))

(defun calibredb-set-read-column (&optional candidate)
  (calibredb-set-custom
   "read_status"
   (calibredb-candidate-key :id candidate)
   "true"))

(defun calibredb-set-custom-at-point (col val)
  (interactive "sColumn: \nsValue: ")
  (calibredb-set-custom
   col
   (calibredb-candidate-key :id candidate)
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

(cl-defun calibredb-toggle-tag-at-point (tag)
  (interactive)
  (let* ((cands (or (calibredb-find-marked-candidates) (calibredb-find-candidate-at-point))))
    (dolist (cand cands)
      (let* ((tags (calibredb-getattr cand :tag))
             (tag-list (split-string tags ",")))
        (setq tag-list
              (if (member tag tag-list)
                  (delete tag tag-list)
                (push tag tag-list)))
        ;; (calibredb-set-metadata-process
        ;;  (list cand)
        ;;  "tags"
        ;;  (string-join tag-list ","))
        (calibredb-set-metadata--tags-server (calibredb-getattr cand :id) tag-list)
        (calibredb-search-refresh-or-resume)
        ))))

(defvar calibredb-read-filter-p nil)
(defun calibredb-filter-toggle-tag (tag)
  "Filter results by tag."
  (interactive)
  (if calibredb-read-filter-p
      (progn
        (calibredb-search-clear-filter)
        (setq-local calibredb-read-filter-p nil))
    (letf! ((defun completing-read (&rest args) tag))
      (calibredb-filter-by-tag)
      (setq-local calibredb-read-filter-p t))))

;; can't access metadata db while server is running so set the
;; server url as library-path when updating metadata
(defadvice! calibredb-set-metadata-process-server (func cands field input)
  :around #'calibredb-set-metadata-process
  (let ((calibredb-root-dir calibredb-opds-root-url))
    (apply func (list cands field input))))

;; calibredb
(map! :desc "calibredb" :leader "o l" #'calibredb)

(map! :map calibredb-search-mode-map
      ;; :ne "C" #'calibredb-set-custom-at-point
      :ne "M-r" (cmd! (calibredb-toggle-tag-at-point "read"))
      :ne "M-e" (cmd! (calibredb-toggle-tag-at-point "active"))
      :ne "M-w" (cmd! (calibredb-toggle-tag-at-point (completing-read "Tag: " (calibredb-all-tag))))
      :ne "C-r" (cmd! (calibredb-filter-toggle-tag "read"))
      :ne "C-e" (cmd! (calibredb-filter-toggle-tag "active")))

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
        calibredb-library-alist '(("http://localhost:8099"))
        calibredb-opds-root-url "http://localhost:8099")
  (calibredb-add-read-column))

(use-package! arxiv-mode)
(map! :mode arxiv-mode
      :localleader
      :ne "n" #'arxiv-read-new
      :ne "r" #'arxiv-read-recent
      :ne "a" #'arxiv-read-author)

(use-package! pdf-tools
  :commands pdf-view-mode
  :config
  (pdf-tools-install))


;; calibredb server usage
(defun calibredb-server-url (&optional path)
  (if calibredb-server-host
      (concat calibredb-server-host ":" calibredb-server-port (or path ""))
    ;; start a calibre-server managed by emacs and set url..
    ;; ...
    ))

;; ;; send request to update metadata
(defun calibredb-set-metadata--tags-server (id tags)
  (request
    (calibredb-server-url (concat "/cdb/set-fields/" id "/books"))
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode `(("changes" . (("tags" . ,tags)))))
    ;; :complete (lambda (&rest args)
    ;;             (prin1 (plist-get args :data)))
    ))

;; CALIBREDB CONTENT SERVER SUPPORT
;; ;; fetch library data
;; (request
;;   (calibredb-server-url "/interface-data/books-init")
;;   :params '(("library_id" . calibredb-library-id)
;;             ("sort" . (concat (symbol-name calibredb-sort-by) "."
;;                               (symbol-name calibredb-order))))
;;   :parser 'json-read
;;   :complete (lambda (&rest args)
;;               (setq-local calibredb-data (plist-get args :data))))

;; ;; parse the books candidates
;; (let ((candidates '()))
;;   (dolist (book (alist-get 'metadata calibredb-data))
;;     (push (let* ((book-id (car book))
;;                  (book-data (cdr book)))
;;             `((:id ,book-id)
;;               (:author-sort ,(alist-get 'author_sort book-data))
;;               (:book-dir nil)
;;               (:book-name ,(alist-get 'sort book-data))
;;               (:book-format ,(downcase (seq-elt (alist-get 'formats book-data) 0)))
;;               (:book-pubdate ,(alist-get 'pubdate book-data))
;;               (:book-title ,(alist-get 'title book-data))
;;               (:file-path nil)
;;               (:tag ,(string-join (alist-get 'tags book-data) ","))
;;               (:size ,(alist-get 'size book-data))
;;               (:comment ,(alist-get 'comments book-data))
;;               (:ids ,(alist-get 'identifiers book-data))
;;               (:series ,(alist-get 'series_index book-data))
;;               (:lang_code ,(seq-elt (alist-get 'languages book-data) 0))))
;;           candidates))
;;   candidates)



(provide 'calibredb-config)
;; calibredb-config ends here
