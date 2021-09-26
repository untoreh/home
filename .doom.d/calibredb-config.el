;;; calibredb-extras.el -*- lexical-binding: t; -*-

(require 'calibredb)
(require 's)
(require 'request)

(defun calibredb-args (&key quotes &rest args)
  (apply #'format `(,(s-repeat (length args) (if quotes "'%s' " "%s "))
                    ,@args)))

;; calibredb server usage
(defun calibredb-server-url (&optional path)
  (concat (or calibredb-server-url
              (setq calibredb-server-url
                    (if calibredb-server-host
                        (concat calibredb-server-host ":" calibredb-server-port)
                      ;; start a calibre-server managed by emacs and set url..
                      ;; ...
                      ))) path))

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
  (format "--library-path '%s'"
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

;; (defadvice! calibredb-override-set-tag (&optional candidates) #'calibredb-set-metadata--tags
;;   (interactive)
;;   (let ((cands (or candidates
;;                   (calibredb-find-candidate-at-point)
;;                   (calibredb-find-marked-candidates))))
;;     (calibredb-set-metadata--tags-server (calibredb-getattr (first cand) :id)
;;                                          (read-string (if (> num 0)
;;                                    (concat "Set " field " for " (number-to-string num) " items: ")
;;                                  (concat prompt id " " title ": ") ) init))
    ;; )
;; (cond ((equal major-mode 'calibredb-show-mode)
;;              (calibredb-show-refresh))
;;             ((eq major-mode 'calibredb-search-mode)
;;              (calibredb-search-refresh-or-resume))
;;             (t nil))
;;   )

(defvar calibredb-toggle-filter-alist '())
(make-variable-buffer-local 'calibredb-toggle-filter-alist)

(defun calibredb-filter-toggle-tag (tag)
  "Filter results by tag."
  (interactive)
  (let ((s-contains?-func (symbol-function #'s-contains?))
        (toggle (alist-get tag calibredb-toggle-filter-alist)))
    (letf! ((defun inv-func (res) (if toggle (not res) res)))
      (letf! ((defun completing-read (&rest args) tag)
              (defun s-contains? (needle s &optional ignore)
                (inv-func (funcall s-contains?-func needle s ignore))))
        (calibredb-filter-by-tag)
        (setf (alist-get tag calibredb-toggle-filter-alist)
              (not toggle))))))

(defun calibredb-arxiv-id-p (id)
  (string-match-p "^[0-9]+\\.[0-9]+\\(\\.[a-z0-9]+\\)?" id))

(defun calibredb-bibtex-field (k data)
  " Get a field from a bibtex entry alist and strip curly braces. "
  (if-let ((val (cdr (assoc k data))))
      (progn
        (substring val 1 -1))
    ""))

(defun calibredb-arxiv-metadata (id)
  (let ((entry (with-temp-buffer
                 (insert (arxiv-get-bibtex-entry-via-arxiv-api id))
                 (goto-char (point-min))
                 (bibtex-parse-entry))))
    (list (cons "Arxiv"
                (remove-if (lambda (x) (string= "" (cdr x)) )
                           `(("Title" . ,(calibredb-bibtex-field "title" entry))
                             ("Authors" . ,(calibredb-bibtex-field "author" entry))
                             ("Publisher" . ,(calibredb-bibtex-field "journal" entry))
                             ("Tags" . ,(calibredb-bibtex-field "primaryClass" entry))
                             ("Comments".  ,(calibredb-bibtex-field "abstract" entry))
                             ("Identifiers" . ,(let ((p (calibredb-bibtex-field "eprint" entry)))
                                                 (if (not (string= "" p))
                                                     (concat "arxiv:" p)
                                                   (concat "arxiv:" id))))))))))

(add-transient-hook! #'arxiv-get-bibtex-entry-via-arxiv-api
  (require 'org-ref-arxiv))

;; (advice-remove 'calibredb-set-content-server #'calibredb-fetch-and-set-metadata-by-author-and-title)
(defadvice! calibredb-set-content-server (func arg)
  :around #'calibredb-fetch-and-set-metadata-by-author-and-title
  (require 'org-ref-arxiv)
  ;; override mapc to avoid using `calibredb-command' to set metadata
  (let* ((cand (car (calibredb-find-candidate-at-point)))
        (id (calibredb-getattr cand :id))
        (title (calibredb-getattr cand :book-title)))
    (cond
     ((calibredb-arxiv-id-p title)
      (let ((metadata (cdar (calibredb-arxiv-metadata title))))
        (cond (metadata
               (progn
                 (calibredb-set-metadata--server id metadata)
                 (let ((window (get-buffer-window "*calibredb-search*")))
                   (if window
                       (select-window window)
                     (switch-to-buffer-other-window "*calibredb-search*")))
                 (calibredb-search-refresh-or-resume)
                 (if calibredb-show-results (calibredb-show-results metadata t))
                 (message (format "Metadata updated: ID - %s, Title - %s" id title))))
              (t (print "No metadata retrieved from sources")))
        ))
     (t (funcall func arg)))))

(defadvice! calibredb-select-metadata-noivy (results) :override #'calibredb-select-metadata-source
  (cdr (assoc (completing-read "Select metadata source : " results) results)))

;; calibredb-add seems to fail if ivy-mode is bound but counsel is not
;; (defadvice! my/calibredb-add (arg) :override #'calibredb-add
;;   (let ((file (read-file-name "Add a file to Calibre: " calibredb-download-dir)))
;;     (calibredb-counsel-add-file-action arg file))
;;   (if (equal major-mode 'calibredb-search-mode)
;;       (calibredb-search-refresh-or-resume)))

(defadvice! calibredb-add-book (func arg) :around #'calibredb-add
  (read-file-name "Add a file to Calibre: " calibredb-download-dir)
  (funcall func arg))

(cl-defun calibredb-func-server (func &key command option input id library action)
  (pcase command
    ("set_metadata" (let* ((input-list (split-string input ":"))
                           (field (nth 0 input-list))
                           (val (string-trim (nth 1 input-list) "\"" "\"")))
                      (calibredb-set-metadata--server id `((,field . ,val)))
                      (cond ((equal major-mode 'calibredb-show-mode)
                             (calibredb-show-refresh))
                            ((eq major-mode 'calibredb-search-mode)
                             (calibredb-search-refresh-or-resume))
                            (t nil))))
    (t (funcall func
                :command command
                :option option
                :input input
                :id id
                :library (format "--with-library \"%s\"" (calibredb-server-url))
                :action action))))

(advice-add #'calibredb-command :around #'calibredb-func-server)
(advice-add #'calibredb-process :around #'calibredb-func-server)

(defun calibredb-pdftitle (&optional candidate)
  (unless (executable-find "pdftitle")
    (error "pdftitle not found in path"))
  (let* ((cand (first (or candidate (calibredb-find-candidate-at-point))))
         (title (calibredb-getattr cand :book-title))
         (file (file-truename (calibredb-getattr cand :file-path)))
         (result (shell-command-to-string
                  (format "pdftitle -p \"%s\" --replace-missing-char \"%s\" "
                          (shell-quote-argument file)
                          (shell-quote-argument " "))))
         (fail (string-match-p "Exception\\|Traceback" result)))
    (if fail
        (error result)
      (let ((new-title (string-replace "\n" "" result)))
        (if (yes-or-no-p (format "Replace book title (%s) with: %s ?" title new-title))
            (progn (calibredb-set-metadata--server
                    (calibredb-getattr cand :id) `(("title" . ,new-title)))
                   (calibredb-search-refresh-or-resume)))))))

;; calibredb
(map! :desc "calibredb" :leader "o l" #'calibredb)

(map! :map calibredb-search-mode-map
      ;; :ne "C" #'calibredb-set-custom-at-point
      :ne "M-r" (cmd! (calibredb-toggle-tag-at-point "read"))
      :ne "M-e" (cmd! (calibredb-toggle-tag-at-point "active"))
      :ne "M-w" (cmd! (calibredb-toggle-tag-at-point (completing-read "Tag: " (calibredb-all-tag))))
      :ne "C-r" (cmd! (calibredb-filter-toggle-tag "read"))
      :ne "C-e" (cmd! (calibredb-filter-toggle-tag "active"))
      :ne "C-p" (cmd! (prin1 (calibredb-pdftitle))))

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
  (setq
   calibredb-root-dir "~/Documents/books/"
   calibredb-download-dir "~/Documents/books/toadd/"
   calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
   calibredb-server-host "http://localhost"
   calibredb-server-port "8099"
   calibredb-server-url nil
   calibredb-library-alist '(("http://localhost:8099/"))
   calibredb-opds-download-dir (expand-file-name
                                temporary-file-directory "calibredb"))
  (calibredb-add-read-column))


;; additional packages
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

(use-package! nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defun calibredb-set-metadata--server (id data &optional sync)
  (let ((changes (mapcar (lambda (x) (cons (downcase (car x)) (cdr x))) data)))
    (request
      (calibredb-server-url (concat "/cdb/set-fields/" id "/books"))
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode `(("changes" . ,changes)))
      :sync sync
      ;; :complete (lambda (&rest args)
      ;;             (prin1 (plist-get args :data)))
      )))

;; ;; send request to update metadata
(defun calibredb-set-metadata--tags-server (id tags)
  (calibredb-set-metadata--server id `(("tags" . ,tags))))

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
