;;; grammatical-edit.el --- Grammatical edit base on tree-sitter -*- lexical-binding: t; -*-

;; Filename: grammatical-edit.el
;; Description: Grammatical edit base on tree-sitter
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-25 21:24:03
;; Version: 0.1
;; Last-Updated: 2021-11-25 21:24:03
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/grammatical-edit
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Grammatical edit base on tree-sitter
;;

;;; Installation:
;;
;; Put grammatical-edit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'grammatical-edit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET grammatical-edit RET
;;

;;; Change log:
;;
;; 2021/11/25
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'subr-x)
(require 'thingatpt)
(require 'tree-sitter)

;;; Code:

(defvar grammatical-edit-mode-map (make-sparse-keymap)
  "Keymap for the grammatical-edit minor mode.")

;;;###autoload
(define-minor-mode grammatical-edit-mode
  "Minor mode for auto parenthesis pairing with syntax table.
\\<grammatical-edit-mode-map>"
  :group 'grammatical-edit)

(defmacro grammatical-edit-ignore-errors (body)
  `(ignore-errors
     ,body
     t))

(defcustom grammatical-edit-save-in-kill-ring t
  "Whether save kill thing into kill-ring."
  :type 'boolean)

;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;

(defun grammatical-edit-delete-region (beg end)
  (if grammatical-edit-save-in-kill-ring
      (kill-region beg end)
    (delete-region beg end)))

(defun grammatical-edit-open-object (object-start object-end)
  (interactive)
  (cond
   ((region-active-p)
    (grammatical-edit-wrap-round))
   ((and (grammatical-edit-in-string-p)
         (derived-mode-p 'js-mode))
    (insert (format "%s%s" object-start object-end))
    (backward-char))
   ((or (grammatical-edit-in-string-p)
        (grammatical-edit-in-comment-p))
    (insert object-start))
   (t
    (insert (format "%s%s" object-start object-end))
    (backward-char))))

(defun grammatical-edit-open-round ()
  (interactive)
  (grammatical-edit-open-object "(" ")"))

(defun grammatical-edit-open-curly ()
  (interactive)
  (grammatical-edit-open-object "{" "}"))

(defun grammatical-edit-open-bracket ()
  (interactive)
  (grammatical-edit-open-object "[" "]"))

(defun grammatical-edit-fix-unbalanced-parentheses ()
  (interactive)
  (let ((close (grammatical-edit-missing-close)))
    (if close
        (cond ((eq ?\) (matching-paren close))
               (insert ")"))
              ((eq ?\} (matching-paren close))
               (insert "}"))
              ((eq ?\] (matching-paren close))
               (insert "]")))
      (up-list))))

(defun grammatical-edit-close-round ()
  (interactive)
  (cond ((or (grammatical-edit-in-string-p)
             (grammatical-edit-in-comment-p))
         (insert ")"))
        ;; Insert ) directly in sh-mode for case ... in syntax.
        ((or
          (derived-mode-p 'sh-mode)
          (derived-mode-p 'markdown-mode))
         (insert ")"))
        (t
         (grammatical-edit-fix-unbalanced-parentheses))))

(defun grammatical-edit-close-curly ()
  (interactive)
  (cond ((or (grammatical-edit-in-string-p)
             (grammatical-edit-in-comment-p))
         (insert "}"))
        (t
         (grammatical-edit-fix-unbalanced-parentheses))))

(defun grammatical-edit-close-bracket ()
  (interactive)
  (cond ((or (grammatical-edit-in-string-p)
             (grammatical-edit-in-comment-p))
         (insert "]"))
        (t
         (grammatical-edit-fix-unbalanced-parentheses))))

(defun grammatical-edit-single-quote ()
  (interactive)
  (cond ((or (grammatical-edit-is-lisp-mode-p)
             (derived-mode-p 'markdown-mode))
         (insert "'"))
        ((region-active-p)
         (grammatical-edit-wrap-single-quote))
        ((grammatical-edit-in-string-p)
         (cond
          ((and (derived-mode-p 'python-mode)
                (and (eq (char-before) ?\') (eq (char-after) ?\')))
           (insert "''")
           (backward-char))
          (t
           (insert "'"))))
        ((grammatical-edit-in-comment-p)
         (insert "'"))
        (t
         (insert "''")
         (backward-char))))

(defun grammatical-edit-double-quote ()
  (interactive)
  (cond ((region-active-p)
         (grammatical-edit-wrap-double-quote))
        ((grammatical-edit-in-single-quote-string-p)
         (insert "\""))
        ((grammatical-edit-in-string-p)
         (cond
          ((and (derived-mode-p 'python-mode)
                (and (eq (char-before) ?\") (eq (char-after) ?\")))
           (insert "\"\"")
           (backward-char))
          ;; When current mode is golang.
          ;; Don't insert \" in string that wrap by `...`
          ((and (derived-mode-p 'go-mode)
                (equal (save-excursion (nth 3 (grammatical-edit-current-parse-state))) 96))
           (insert "\""))
          (t
           (insert "\\\""))))
        ((grammatical-edit-in-comment-p)
         (insert "\""))
        (t
         (insert "\"\"")
         (backward-char))))

(defun grammatical-edit-space (arg)
  "Wrap space around cursor if cursor in blank parenthesis.

input: {|} (press <SPACE> at |)
output: { | }

input: [|] (press <SPACE> at |)
output: [ | ]
"
  (interactive "p")
  (if (> arg 1)
      (self-insert-command arg)
    (cond ((or (grammatical-edit-in-comment-p)
               (grammatical-edit-in-string-p))
           (insert " "))
          ((or (and (equal (char-after) ?\} )
                    (equal (char-before) ?\{ ))
               (and (equal (char-after) ?\] )
                    (equal (char-before) ?\[ )))
           (insert "  ")
           (backward-char 1))
          (t
           (insert " ")))))

(defun grammatical-edit-web-mode-match-paren ()
  (require 'sgml-mode)
  (cond ((looking-at "<")
         (sgml-skip-tag-forward 1))
        ((looking-back ">")
         (sgml-skip-tag-backward 1))
        (t (self-insert-command (or arg 1)))))

(defun grammatical-edit-backward-delete ()
  (interactive)
  (cond ((grammatical-edit-in-string-p)
         (grammatical-edit-backward-delete-in-string))
        ((grammatical-edit-in-comment-p)
         (backward-delete-char 1))
        ((grammatical-edit-after-close-pair-p)
         (if (and (derived-mode-p 'sh-mode)
                  (eq ?\) (char-before)))
             (delete-char -1)
           (backward-char)))
        ((grammatical-edit-in-empty-pair-p)
         (grammatical-edit-backward-delete-in-pair))
        ((not (grammatical-edit-after-open-pair-p))
         (backward-delete-char 1))))

(defun grammatical-edit-forward-delete ()
  (interactive)
  (cond ((region-active-p)
         (grammatical-edit-delete-region (region-beginning) (region-end)))
        ((grammatical-edit-in-empty-backquote-string-p)
         (grammatical-edit-delete-empty-backquote-string))
        ((grammatical-edit-in-empty-string-p)
         (grammatical-edit-delete-empty-string))
        ((grammatical-edit-in-string-p)
         (grammatical-edit-forward-delete-in-string))
        ((grammatical-edit-in-comment-p)
         (delete-char 1))
        ((grammatical-edit-before-string-open-quote-p)
         (grammatical-edit-forward-movein-string))
        ((grammatical-edit-before-open-pair-p)
         (forward-char))
        ((grammatical-edit-in-empty-pair-p)
         (grammatical-edit-backward-delete-in-pair))
        ((and (derived-mode-p 'sh-mode)
              (grammatical-edit-before-close-pair-p)
              (eq ?\) (char-after)))
         (delete-char 1))
        ((not (grammatical-edit-before-close-pair-p))
         (delete-char 1))))

(defun grammatical-edit-kill ()
  "Intelligent soft kill.

When inside of code, kill forward S-expressions on the line, but respecting delimeters.
When in a string, kill to the end of the string.
When in comment, kill to the end of the line."
  (interactive)
  (cond ((region-active-p)
         (grammatical-edit-delete-region (region-beginning) (region-end)))
        ((derived-mode-p 'web-mode)
         (grammatical-edit-web-mode-kill))
        (t
         (grammatical-edit-common-mode-kill))))

(defun grammatical-edit-backward-kill ()
  "Intelligent soft kill.
When inside of code, kill backward S-expressions on the line, but respecting delimiters.
When in a string, kill to the beginning of the string.
When in comment, kill to the beginning of the line."
  (interactive)
  (cond ((derived-mode-p 'web-mode)
         (grammatical-edit-web-mode-backward-kill))
        (t
         (grammatical-edit-common-mode-backward-kill))))

(defun grammatical-edit-wrap-double-quote ()
  (interactive)
  (cond ((and (region-active-p)
              (grammatical-edit-in-string-p))
         (cond ((and (derived-mode-p 'go-mode)
                     (equal (save-excursion (nth 3 (grammatical-edit-current-parse-state))) 96))
                (grammatical-edit-wrap-region "\"" "\""))
               (t
                (grammatical-edit-wrap-region "\\\"" "\\\""))))
        ((region-active-p)
         (grammatical-edit-wrap-region "\"" "\""))
        ((grammatical-edit-in-string-p)
         (goto-char (cdr (grammatical-edit-current-node-range))))
        ((grammatical-edit-in-comment-p)
         (grammatical-edit-wrap (beginning-of-thing 'symbol) (end-of-thing 'symbol) "\"" "\""))
        ((grammatical-edit-is-lisp-mode-p)
         (grammatical-edit-wrap (beginning-of-thing 'sexp) (end-of-thing 'sexp) "\"" "\""))
        (t
         (grammatical-edit-wrap (beginning-of-thing 'symbol) (end-of-thing 'symbol) "\"" "\""))))

(defun grammatical-edit-wrap-single-quote ()
  (interactive)
  (cond ((region-active-p)
         (grammatical-edit-wrap-region "'" "'"))
        ((grammatical-edit-in-comment-p)
         (grammatical-edit-wrap (beginning-of-thing 'symbol) (end-of-thing 'symbol) "'" "'"))
        ((grammatical-edit-is-lisp-mode-p)
         (grammatical-edit-wrap (beginning-of-thing 'sexp) (end-of-thing 'sexp) "'" "'"))
        (t
         (grammatical-edit-wrap (beginning-of-thing 'symbol) (end-of-thing 'symbol) "'" "'"))))

(defun grammatical-edit-wrap-round ()
  (interactive)
  (cond
   ;; If in *.Vue file
   ;; In template area, call `grammatical-edit-web-mode-element-wrap'
   ;; Otherwise, call `grammatical-edit-wrap-round-pair'
   ((and (buffer-file-name) (string-equal (file-name-extension (buffer-file-name)) "vue"))
    (if (grammatical-edit-vue-in-template-area-p)
        (grammatical-edit-web-mode-element-wrap)
      (grammatical-edit-wrap-round-pair)))
   ;; If is `web-mode' but not in *.Vue file, call `grammatical-edit-web-mode-element-wrap'
   ((derived-mode-p 'web-mode)
    (if (grammatical-edit-in-script-area-p)
        (grammatical-edit-wrap-round-pair)
      (grammatical-edit-web-mode-element-wrap)))
   ;; Otherwise call `grammatical-edit-wrap-round-pair'
   (t
    (grammatical-edit-wrap-round-pair))))

(defun grammatical-edit-wrap-round-object (object-start object-end)
  (cond ((region-active-p)
         (grammatical-edit-wrap-region object-start object-end))
        ((grammatical-edit-in-comment-p)
         (grammatical-edit-wrap (beginning-of-thing 'symbol) (end-of-thing 'symbol) object-start object-end))
        ((grammatical-edit-is-lisp-mode-p)
         (grammatical-edit-wrap (beginning-of-thing 'sexp) (end-of-thing 'sexp) object-start object-end))
        ((member (grammatical-edit-node-type-at-point) (list "{" "(" "["))
         (let ((match-paren-pos (save-excursion
                                  (grammatical-edit-match-paren 1)
                                  (point))))
           (grammatical-edit-wrap (point) match-paren-pos object-start object-end)))
        (t
         (when (grammatical-edit-before-string-open-quote-p)
           (grammatical-edit-forward-movein-string))
         (let ((string-bound (grammatical-edit-current-node-range)))
           (grammatical-edit-wrap (car string-bound) (cdr string-bound) object-start object-end))))

  (unless (or (grammatical-edit-in-string-p)
              (grammatical-edit-in-comment-p))
    ;; Indent wrap area.
    (grammatical-edit-indent-parent-area)

    ;; Backward char if cursor in nested roud, such as `( ... )|)`
    (when (grammatical-edit-nested-round-p)
      (backward-char 1))
    ;; Jump to start position of parent node.
    (goto-char (tsc-node-start-position (tsc-get-parent (tree-sitter-node-at-point))))))

(defun grammatical-edit-is-lisp-mode-p ()
  (or (derived-mode-p 'lisp-mode)
      (derived-mode-p 'emacs-lisp-mode)
      (derived-mode-p 'inferior-emacs-lisp-mode)))

(defun grammatical-edit-nested-round-p ()
  (save-excursion
    (backward-char 1)
    (let ((node-type (grammatical-edit-node-type-at-point)))
      (or (string-equal node-type ")")
          (string-equal node-type "]")
          (string-equal node-type "}")))))

(defun grammatical-edit-wrap-round-pair ()
  (interactive)
  (grammatical-edit-wrap-round-object "(" ")"))

(defun grammatical-edit-wrap-bracket ()
  (interactive)
  (grammatical-edit-wrap-round-object "[" "]"))

(defun grammatical-edit-wrap-curly ()
  (interactive)
  (grammatical-edit-wrap-round-object "{" "}"))

(defun grammatical-edit-unwrap (&optional argument)
  (interactive "P")
  (cond ((derived-mode-p 'web-mode)
         (grammatical-edit-web-mode-element-unwrap))
        ((grammatical-edit-in-string-p)
         (grammatical-edit-unwrap-string argument))
        (t
         (save-excursion
           (grammatical-edit-kill-surrounding-sexps-for-splice argument)
           (backward-up-list)
           (save-excursion
             (forward-sexp)
             (backward-delete-char 1))
           (delete-char 1)
           ;; Try to indent parent expression after unwrap pair.
           ;; This feature just enable in lisp-like language.
           (when (grammatical-edit-is-lisp-mode-p)
             (ignore-errors
               (backward-up-list)
               (indent-sexp)))))))

(defun grammatical-edit-jump-out-pair-and-newline ()
  (interactive)
  (cond ((grammatical-edit-in-string-p)
         (goto-char (cdr (grammatical-edit-current-node-range)))
         (newline-and-indent))
        (t
         ;; Just do when have `up-list' in next step.
         (if (grammatical-edit-ignore-errors (save-excursion (up-list)))
             (let (up-list-point)
               (if (grammatical-edit-is-blank-line-p)
                   ;; Clean current line first if current line is blank line.
                   (grammatical-edit-kill-current-line)
                 ;; Move out of current parentheses and newline.
                 (up-list)
                 (setq up-list-point (point))
                 (newline-and-indent)
                 ;; Try to clean unnecessary whitespace before close parenthesis.
                 ;; This feature just enable in lisp-like language.
                 (when (grammatical-edit-is-lisp-mode-p)
                   (save-excursion
                     (goto-char up-list-point)
                     (backward-char)
                     (when (grammatical-edit-only-whitespaces-before-cursor-p)
                       (grammatical-edit-delete-whitespace-around-cursor))))))
           ;; Try to clean blank line if no pair can jump out.
           (if (grammatical-edit-is-blank-line-p)
               (grammatical-edit-kill-current-line))))))

(defun grammatical-edit-jump-left ()
  (interactive)
  (let* ((current-node (tree-sitter-node-at-point))
         (prev-node (tsc-get-prev-sibling current-node))
         (current-node-text (tsc-node-text current-node))
         (current-point (point)))
    (cond
     ;; Skip blank space.
     ((looking-back "\\s-+")
      (search-backward-regexp "[^ \t\n]" nil t))

     ;; Jump to previous non-blank char if at line beginng.
     ((bolp)
      (previous-line 1)
      (end-of-line)
      (search-backward-regexp "[^ \t\n]" nil t))

     ;; Jump to previous open char.
     ((and (eq major-mode 'web-mode)
           (eq (tsc-node-type current-node) 'raw_text))
      (backward-char 1)
      (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))

     ;; Jump out string if in string.
     ((grammatical-edit-in-string-p)
      (goto-char (tsc-node-start-position current-node)))

     ;; Jump to node start position if current node exist.
     ((> (length current-node-text) 0)
      (goto-char (tsc-node-start-position current-node))
      (if (equal (point) current-point)
          (backward-char 1)))

     ;; Otherwise, jump to start position of previous node.
     (prev-node
      (goto-char (tsc-node-start-position prev-node))))))

(defun grammatical-edit-jump-right ()
  (interactive)
  (let* ((current-node (tree-sitter-node-at-point))
         (next-node (tsc-get-next-sibling current-node))
         (current-node-text (tsc-node-text current-node))
         (current-point (point)))
    (cond
     ;; Skip blank space.
     ((looking-at "\\s-+")
      (search-forward-regexp "\\s-+" nil t))

     ;; Jump to next non-blank char if at line end.
     ((eolp)
      (next-line 1)
      (beginning-of-line)
      (search-forward-regexp "\\s-+" nil t))

     ;; Jump into string if at before string open quote char.
     ((eq (char-after) ?\")
      (forward-char))

     ;; Jump to next close char.
     ((and (eq major-mode 'web-mode)
           (eq (tsc-node-type current-node) 'raw_text))
      (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
      (forward-char 1))

     ;; Jump out string if in string.
     ((grammatical-edit-in-string-p)
      (goto-char (tsc-node-end-position current-node)))

     ;; Jump to node end position if current node exist.
     ((> (length current-node-text) 0)
      (goto-char (tsc-node-end-position current-node))
      (if (equal (point) current-point)
          (forward-char 1)))

     ;; Otherwise, jump to end position of next node.
     (next-node
      (goto-char (tsc-node-end-position next-node))))))

(defun grammatical-edit-delete-whitespace-around-cursor ()
  (grammatical-edit-delete-region (save-excursion
                                    (search-backward-regexp "[^ \t\n]" nil t)
                                    (forward-char)
                                    (point))
                                  (save-excursion
                                    (search-forward-regexp "[^ \t\n]" nil t)
                                    (backward-char)
                                    (point))))

(defun grammatical-edit-kill-current-line ()
  (grammatical-edit-delete-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun grammatical-edit-missing-close ()
  (let ((start-point (point))
        open)
    (save-excursion
      ;; Get open tag.
      (backward-up-list)
      (setq open (char-after))

      ;; Jump to start position and use `check-parens' check unbalance paren.
      (goto-char start-point)
      (ignore-errors
        (check-parens))

      ;; Return missing tag if point change after `check-parens'
      ;; Otherwhere return nil.
      (if (equal start-point (point))
          nil
        open))))

(defun grammatical-edit-backward-delete-in-pair ()
  (backward-delete-char 1)
  (delete-char 1))

(defun grammatical-edit-forward-movein-string ()
  (cond ((and (eq (grammatical-edit-node-type-at-point) 'raw_string_literal)
              (eq (char-after) ?`))
         (forward-char 1))
        (t
         (forward-char (length (tsc-node-text (tree-sitter-node-at-point)))))))

(defun grammatical-edit-is-string-node-p (current-node)
  (or (eq (tsc-node-type current-node) 'string)
      (eq (tsc-node-type current-node) 'string_literal)
      (eq (tsc-node-type current-node) 'interpreted_string_literal)
      (eq (tsc-node-type current-node) 'raw_string_literal)
      (string-equal (tsc-node-type current-node) "\"")))

(defun grammatical-edit-in-empty-backquote-string-p ()
  (let ((current-node (tree-sitter-node-at-point)))
    (and (grammatical-edit-is-string-node-p current-node)
         (string-equal (tsc-node-text current-node) "``")
         (eq (char-before) ?`)
         (eq (char-after) ?`)
         )))

(defun grammatical-edit-get-parent-bound-info ()
  (let* ((current-node (tree-sitter-node-at-point))
         (parent-node (tsc-get-parent current-node))
         (parent-bound-start (tsc-node-text (save-excursion
                                              (goto-char (tsc-node-start-position parent-node))
                                              (tree-sitter-node-at-point))))
         (parent-bound-end (tsc-node-text (save-excursion
                                            (goto-char (tsc-node-end-position parent-node))
                                            (backward-char 1)
                                            (tree-sitter-node-at-point)))))
    (list current-node parent-node parent-bound-start parent-bound-end)))

(defun grammatical-edit-in-empty-string-p ()
  (or (let* ((parent-bound-info (grammatical-edit-get-parent-bound-info))
             (current-node (nth 0 parent-bound-info))
             (parent-node (nth 1 parent-bound-info))
             (string-bound-start (nth 2 parent-bound-info))
             (string-bound-end (nth 3 parent-bound-info)))
        (and (grammatical-edit-is-string-node-p current-node)
             (= (length (tsc-node-text parent-node)) (+ (length string-bound-start) (length string-bound-end)))
             ))
      (string-equal (tsc-node-text (tree-sitter-node-at-point)) "\"\"")))

(defun grammatical-edit-backward-delete-in-string ()
  (cond
   ;; Delete empty string if cursor in empty string.
   ((grammatical-edit-in-empty-backquote-string-p)
    (grammatical-edit-delete-empty-backquote-string))
   ((grammatical-edit-in-empty-string-p)
    (grammatical-edit-delete-empty-string))
   ;; Jump left to out of string quote if cursor after open quote.
   ((grammatical-edit-after-open-quote-p)
    (backward-char (length (save-excursion
                             (backward-char 1)
                             (tsc-node-text (tree-sitter-node-at-point))))))
   ;; Delete previous character.
   (t
    (backward-delete-char 1))))

(defun grammatical-edit-delete-empty-string ()
  (cond ((string-equal (tsc-node-text (tree-sitter-node-at-point)) "\"\"")
         (grammatical-edit-delete-region (- (point) 1) (+ (point) 1)))
        (t
         (let* ((current-node (tree-sitter-node-at-point))
                (node-bound-length (save-excursion
                                     (goto-char (tsc-node-start-position current-node))
                                     (length (tsc-node-text (tree-sitter-node-at-point))))))
           (grammatical-edit-delete-region (- (point) node-bound-length) (+ (point) node-bound-length))))))

(defun grammatical-edit-delete-empty-backquote-string ()
  (grammatical-edit-delete-region (save-excursion
                                    (backward-char 1)
                                    (point))
                                  (save-excursion
                                    (forward-char 1)
                                    (point))))

(defun grammatical-edit-forward-delete-in-string ()
  (let* ((current-node (tree-sitter-node-at-point))
         (node-bound-length (save-excursion
                              (goto-char (tsc-node-start-position current-node))
                              (length (tsc-node-text (tree-sitter-node-at-point))))))
    (unless (eq (point) (- (tsc-node-end-position current-node) node-bound-length))
      (delete-forward-char 1))))

(defun grammatical-edit-unwrap-string (argument)
  (let ((original-point (point))
        (start+end (grammatical-edit-current-node-range)))
    (let ((start (car start+end))
          (end (1- (cdr start+end))))
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (grammatical-edit-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string.")
          (save-excursion
            (goto-char start)
            (grammatical-edit-delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun grammatical-edit-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun grammatical-edit-point-at-sexp-end ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun grammatical-edit-point-at-sexp-boundary (n)
  (cond ((< n 0) (grammatical-edit-point-at-sexp-start))
        ((= n 0) (point))
        ((> n 0) (grammatical-edit-point-at-sexp-end))))

(defun grammatical-edit-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (grammatical-edit-in-string-p)
             (grammatical-edit-in-comment-p))
         (error "Invalid context for splicing S-expressions."))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (grammatical-edit-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (ignore-errors (backward-sexp argument))
           (grammatical-edit-hack-kill-region saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)
               (let ((end (point)))
                 (ignore-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (grammatical-edit-hack-kill-region (point) end))
             (let ((beginning (point)))
               (ignore-errors
                 (while (not (eobp))
                   (forward-sexp)))
               (grammatical-edit-hack-kill-region beginning (point))))))
        (t (error "Bizarre prefix argument `%s'." argument))))

(defun grammatical-edit-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (condition-case condition
        (progn
          (check-parens)
          (buffer-string))
      (error nil))))

(defun grammatical-edit-hack-kill-region (start end)
  (let ((this-command nil)
        (last-command nil))
    (grammatical-edit-delete-region start end)))

(defun grammatical-edit-backward-kill-internal ()
  (cond (current-prefix-arg
         (kill-line (if (integerp current-prefix-arg)
                        current-prefix-arg
                      1)))
        ((grammatical-edit-in-string-p)
         (grammatical-edit-kill-before-in-string))
        ((or (grammatical-edit-in-comment-p)
             (save-excursion
               (grammatical-edit-skip-whitespace nil (point-at-bol))
               (bolp)))
         (if (bolp) (grammatical-edit-backward-delete)
           (kill-line 0)))
        (t (grammatical-edit-kill-sexps-backward-on-line))))

(defun grammatical-edit-js-mode-kill-rest-string ()
  (grammatical-edit-delete-region (point)
                                  (save-excursion
                                    (forward-sexp)
                                    (backward-char)
                                    (point))))

(defun grammatical-edit-at-raw-string-begin-p ()
  (let ((current-node (tree-sitter-node-at-point)))
    (and (grammatical-edit-is-string-node-p current-node)
         (= (point) (1+ (tsc-node-start-position current-node)))
         (or (eq (char-before) ?R)
             (eq (char-before) ?r)
             ))))

(defun grammatical-edit-kill-after-in-string ()
  (let* ((parent-bound-info (grammatical-edit-get-parent-bound-info))
         (current-node (nth 0 parent-bound-info))
         (current-node-bound-end (tsc-node-text (save-excursion
                                                  (goto-char (tsc-node-end-position current-node))
                                                  (backward-char 1)
                                                  (tree-sitter-node-at-point)))))
    (cond ((grammatical-edit-at-raw-string-begin-p)
           (grammatical-edit-delete-region (tsc-node-start-position current-node) (tsc-node-end-position current-node)))
          ((string-equal current-node-bound-end "'''")
           (grammatical-edit-delete-region (point) (- (tsc-node-end-position current-node) (length current-node-bound-end))))
          (t
           (grammatical-edit-delete-region (point) (- (tsc-node-end-position current-node) 1))))))

(defun grammatical-edit-kill-before-in-string ()
  (grammatical-edit-delete-region (point) (1+ (tsc-node-start-position (tree-sitter-node-at-point)))))

(defun grammatical-edit-skip-whitespace (trailing-p &optional limit)
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n"
           limit))

(defun grammatical-edit-kill-sexps-on-line ()
  (if (grammatical-edit-in-char-p)
      (backward-char 2))
  (let ((begin-point (point))
        (eol (point-at-eol)))
    (let ((end-of-list-p (grammatical-edit-forward-sexps-to-kill begin-point eol)))
      (when end-of-list-p
        (up-list)
        (backward-char))
      (goto-char (if (and (not end-of-list-p) (eq (point-at-eol) eol))
                     eol
                   (point)))
      ;; NOTE:                          ; ; ; ; ;
      ;; Back to previous line if kill end point at beginng of line.
      (when (bolp)
        (backward-char 1))
      (grammatical-edit-delete-region begin-point (point)))))

(defun grammatical-edit-kill-sexps-backward-on-line ()
  (if (grammatical-edit-in-char-p)
      (forward-char 1))
  (let ((beginning (point))
        (bol (point-at-bol)))
    (let ((beg-of-list-p (grammatical-edit-backward-sexps-to-kill beginning bol)))
      (when beg-of-list-p
        (up-list -1)
        (forward-char))
      (grammatical-edit-delete-region (if (and (not beg-of-list-p) (eq (point-at-bol) bol))
                                          bol
                                        (point))
                                      beginning))))

(defun grammatical-edit-forward-sexps-to-kill (beginning eol)
  (let ((end-of-list-p nil)
        (firstp t))
    (catch 'return
      (while t
        (save-excursion
          (unless (grammatical-edit-ignore-errors (forward-sexp))
            (when (grammatical-edit-ignore-errors (up-list))
              (setq end-of-list-p (eq (point-at-eol) eol))
              (throw 'return nil)))
          (if (or (and (not firstp)
                       (eobp))
                  (not (grammatical-edit-ignore-errors (backward-sexp)))
                  (not (eq (point-at-eol) eol)))
              (throw 'return nil)))
        (forward-sexp)
        (if (and firstp
                 (eobp))
            (throw 'return nil))
        (setq firstp nil)))
    end-of-list-p))

(defun grammatical-edit-backward-sexps-to-kill (beginning bol)
  (let ((beg-of-list-p nil)
        (lastp t))
    (catch 'return
      (while t
        (save-excursion
          (unless (grammatical-edit-ignore-errors (backward-sexp))
            (when (grammatical-edit-ignore-errors (up-list -1))
              (setq beg-of-list-p (eq (point-at-bol) bol))
              (throw 'return nil)))
          (if (or (and (not lastp)
                       (bobp))
                  (not (grammatical-edit-ignore-errors (forward-sexp)))
                  (not (eq (point-at-bol) bol)))
              (throw 'return nil)))
        (backward-sexp)
        (if (and lastp
                 (bobp))
            (throw 'return nil))
        (setq lastp nil)))
    beg-of-list-p))

(defun grammatical-edit-common-mode-kill ()
  (cond ((grammatical-edit-is-blank-line-p)
         (grammatical-edit-kill-blank-line-and-reindent))
        (current-prefix-arg
         (kill-line (if (integerp current-prefix-arg)
                        current-prefix-arg
                      1)))
        ((grammatical-edit-in-string-p)
         (grammatical-edit-kill-after-in-string))
        ((or (grammatical-edit-in-comment-p)
             (save-excursion
               (grammatical-edit-skip-whitespace t (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp))))
         (kill-line))
        (t (grammatical-edit-kill-sexps-on-line))))

(defun grammatical-edit-common-mode-backward-kill ()
  (if (grammatical-edit-is-blank-line-p)
      (grammatical-edit-ignore-errors
       (progn
         (grammatical-edit-kill-blank-line-and-reindent)
         (forward-line -1)
         (end-of-line)))
    (grammatical-edit-backward-kill-internal)))

(defun grammatical-edit-kill-parent-node ()
  (let ((range (tsc-node-position-range (tsc-get-parent (tree-sitter-node-at-point)))))
    (grammatical-edit-delete-region (car range) (cdr range))))

(defun grammatical-edit-kill-grandfather-node ()
  (let ((range (tsc-node-position-range (tsc-get-parent (tsc-get-parent (tree-sitter-node-at-point))))))
    (grammatical-edit-delete-region (car range) (cdr range))))

(defun grammatical-edit-kill-prepend-space ()
  (grammatical-edit-delete-region (save-excursion
                                    (search-backward-regexp "[^ \t\n]" nil t)
                                    (forward-char 1)
                                    (point))
                                  (point)))

(defun grammatical-edit-at-tag-right (tag)
  (save-excursion
    (backward-char 1)
    (eq (grammatical-edit-node-type-at-point) tag)))

(defun grammatical-edit-web-mode-kill ()
  "It's a smarter kill function for `web-mode'."
  (if (grammatical-edit-is-blank-line-p)
      (grammatical-edit-kill-blank-line-and-reindent)
    (cond
     ;; Kill from current point to attribute end position.
     ((eq (grammatical-edit-node-type-at-point) 'attribute_value)
      (grammatical-edit-delete-region (point) (tsc-node-end-position (tree-sitter-node-at-point))))

     ;; Kill parent node if cursor at attribute or directive node.
     ((or (eq (grammatical-edit-node-type-at-point) 'attribute_name)
          (eq (grammatical-edit-node-type-at-point) 'directive_name))
      (grammatical-edit-kill-parent-node))

     ;; Jump to next non-blank char if in tag area.
     ((eq (grammatical-edit-node-type-at-point) 'self_closing_tag)
      (search-forward-regexp "\\s-+"))

     ;; Clean blank spaces before close tag.
     ((string-equal (grammatical-edit-node-type-at-point) "/>")
      (cond ((looking-back "\\s-")
             (grammatical-edit-kill-prepend-space))
            ;; Kill tag if nothing in tag area.
            ((grammatical-edit-at-tag-right 'tag_name)
             (backward-char 1)
             (grammatical-edit-kill-parent-node))
            (t
             (message "Nothing to kill in tag. ;)"))))

     ;; Clean blank spaces before start tag.
     ((string-equal (grammatical-edit-node-type-at-point) ">")
      (cond ((looking-back "\\s-")
             (grammatical-edit-kill-prepend-space))
            ;; Kill tag content if nothing in tag area.
            ((grammatical-edit-at-tag-right 'tag_name)
             (backward-char 1)
             (grammatical-edit-kill-grandfather-node))
            (t
             (message "Nothing to kill in tag. ;)"))))

     ;; Clean blank space before </
     ((string-equal (grammatical-edit-node-type-at-point) "</")
      (cond ((looking-back "\\s-")
             (grammatical-edit-kill-prepend-space))
            ;; Kill tag content if nothing in tag area.
            ((grammatical-edit-at-tag-right ">")
             (backward-char 1)
             (grammatical-edit-kill-grandfather-node))
            (t
             (message "Nothing to kill in tag. ;)"))))

     ;; Kill all tag content if cursor in tag start area.
     ((eq (grammatical-edit-node-type-at-point) 'tag_name)
      (grammatical-edit-kill-parent-node))

     ;; Kill tag content if cursor at left of <
     ((string-equal (grammatical-edit-node-type-at-point) "<")
      (grammatical-edit-kill-grandfather-node))

     ;; Kill string if cursor at start of quote.
     ((string-equal (grammatical-edit-node-type-at-point) "\"")
      (forward-char 1)
      (grammatical-edit-kill-parent-node))

     ;; Kill content if in start_tag area.
     ((eq (grammatical-edit-node-type-at-point) 'start_tag)
      (cond ((looking-at "\\s-")
             (search-forward-regexp "\\s-+"))
            ((save-excursion
               (grammatical-edit-skip-whitespace t (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp)))
             (kill-line))))

     ;; JavaScript string not identify by tree-sitter.
     ;; We need use `grammatical-edit-current-parse-state' test cursor is in string.
     ((and (eq (grammatical-edit-node-type-at-point) 'raw_text)
           (save-excursion (nth 3 (grammatical-edit-current-parse-state))))
      (grammatical-edit-js-mode-kill-rest-string))

     ;; Use common kill at last.
     (t
      (grammatical-edit-common-mode-kill)))))

(defun grammatical-edit-web-mode-backward-kill ()
  (message "Backward kill in web-mode is currently not implemented."))

(defun grammatical-edit-kill-blank-line-and-reindent ()
  (grammatical-edit-delete-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun grammatical-edit-indent-parent-area ()
  (let ((range (tsc-node-position-range (tsc-get-parent (tree-sitter-node-at-point)))))
    (indent-region (car range) (cdr range))))

(defun grammatical-edit-equal ()
  (interactive)
  (cond
   ((derived-mode-p 'web-mode)
    (cond ((or (eq (grammatical-edit-node-type-at-point) 'attribute_value)
               (eq (grammatical-edit-node-type-at-point) 'raw_text)
               (eq (grammatical-edit-node-type-at-point) 'text))
           (insert "="))
          ;; Insert equal and double quotes if in tag attribute area.
          ((and (string-equal (file-name-extension (buffer-file-name)) "vue")
                (grammatical-edit-vue-in-template-area-p)
                (or (eq (grammatical-edit-node-type-at-point) 'directive_name)
                    (eq (grammatical-edit-node-type-at-point) 'attribute_name)
                    (eq (grammatical-edit-node-type-at-point) 'start_tag)))
           (insert "=\"\"")
           (backward-char 1))
          (t
           (insert "="))))
   (t
    (insert "="))))

(defun grammatical-edit-in-script-area-p ()
  (and (save-excursion
         (search-backward-regexp "<script" nil t))
       (save-excursion
         (search-forward-regexp "</script>" nil t))))

(defun grammatical-edit-vue-in-template-area-p ()
  (and (save-excursion
         (search-backward-regexp "<template>" nil t))
       (save-excursion
         (search-forward-regexp "</template>" nil t))))

(defun grammatical-edit-web-mode-element-wrap ()
  "Like `web-mode-element-wrap', but jump after tag for continue edit."
  (interactive)
  (let (beg end pos tag beg-sep)
    ;; Insert tag pair around select area.
    (save-excursion
      (setq tag (read-from-minibuffer "Tag name? "))
      (setq pos (point))
      (cond
       (mark-active
        (setq beg (region-beginning))
        (setq end (region-end)))
       ((get-text-property pos 'tag-type)
        (setq beg (web-mode-element-beginning-position pos)
              end (1+ (web-mode-element-end-position pos))))
       ((setq beg (web-mode-element-parent-position pos))
        (setq end (1+ (web-mode-element-end-position pos)))))
      (when (and beg end (> end 0))
        (web-mode-insert-text-at-pos (concat "</" tag ">") end)
        (web-mode-insert-text-at-pos (concat "<" tag ">") beg)))

    (when (and beg end)
      ;; Insert return after start tag if have text after start tag.
      (setq beg-sep "")
      (goto-char (+ beg (length (concat "<" tag ">"))))
      (unless (looking-at "\\s-*$")
        (setq beg-sep "\n")
        (insert "\n"))

      ;; Insert return before end tag if have text before end tag.
      (goto-char (+ end (length (concat "<" tag ">")) (length beg-sep)))
      (unless (looking-back "^\\s-*")
        (insert "\n"))

      ;; Insert return after end tag if have text after end tag.
      (goto-char beg)
      (goto-char (+ 1 (web-mode-element-end-position (point))))
      (unless (looking-at "\\s-*$")
        (insert "\n"))

      ;; Indent tag area.
      (let ((indent-beg beg)
            (indent-end (save-excursion
                          (goto-char beg)
                          (+ 1 (web-mode-element-end-position (point)))
                          )))
        (indent-region indent-beg indent-end))

      ;; Jump to start tag, ready for insert tag attributes.
      (goto-char beg)
      (back-to-indentation)
      (forward-char (+ 1 (length tag))))))

(defun grammatical-edit-web-mode-element-unwrap ()
  "Like `web-mode-element-vanish', but you don't need jump parent tag to unwrap.
Just like `paredit-splice-sexp+' style."
  (interactive)
  (save-excursion
    (web-mode-element-parent)
    (web-mode-element-vanish 1)
    (back-to-indentation)))

(defun grammatical-edit-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((or (grammatical-edit-in-comment-p)
             (grammatical-edit-in-string-p))
         (self-insert-command (or arg 1)))
        ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t
         (cond
          ;; Enhancement the automatic jump of web-mode.
          ((derived-mode-p 'web-mode)
           (grammatical-edit-web-mode-match-paren))
          (t
           (self-insert-command (or arg 1)))))))

;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;

(defun grammatical-edit-wrap (beg end a b)
  "Insert A at position BEG, and B after END. Save previous point position.

A and B are strings."
  (goto-char end)
  (insert b)
  (goto-char beg)
  (insert a))

(defun grammatical-edit-wrap-region (a b)
  "When a region is active, insert A and B around it, and jump after A.

A and B are strings."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char end)
      (insert b)
      (goto-char start)
      (insert a))))

(defun grammatical-edit-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (min (point) point)
                        (max (point) point))))

(defun grammatical-edit-current-node-range ()
  (tsc-node-position-range (tree-sitter-node-at-point)))

(defun grammatical-edit-after-open-pair-p ()
  (unless (bobp)
    (save-excursion
      (let ((syn (char-syntax (char-before))))
        (or (eq syn ?\()
            (and (eq syn ?_)
                 (eq (char-before) ?\{)))
        ))))

(defun grammatical-edit-after-close-pair-p ()
  (unless (bobp)
    (save-excursion
      (let ((syn (char-syntax (char-before))))
        (or (eq syn ?\) )
            (eq syn ?\" )
            (and (eq syn ?_ )
                 (eq (char-before) ?\})))))))

(defun grammatical-edit-before-open-pair-p ()
  (unless (eobp)
    (save-excursion
      (let ((syn (char-syntax (char-after))))
        (or (eq syn ?\( )
            (and (eq syn ?_)
                 (eq (char-after) ?\{)))))))

(defun grammatical-edit-before-close-pair-p ()
  (unless (eobp)
    (save-excursion
      (let ((syn (char-syntax (char-after))))
        (or (eq syn ?\) )
            (and (eq syn ?_)
                 (eq (char-after) ?\})))))))

(defun grammatical-edit-in-empty-pair-p ()
  (ignore-errors
    (save-excursion
      (or (and (eq (char-syntax (char-before)) ?\()
               (eq (char-after) (matching-paren (char-before))))
          (and (eq (char-syntax (char-before)) ?_)
               (eq (char-before) ?\{)
               (eq (char-syntax (char-after)) ?_)
               (eq (char-after) ?\})
               )))))

(defun grammatical-edit-node-type-at-point ()
  (ignore-errors (tsc-node-type (tree-sitter-node-at-point))))

(defun grammatical-edit-in-string-p ()
  (ignore-errors
    (or
     ;; If node type is 'string, point must at right of string open quote.
     (let ((current-node (tree-sitter-node-at-point)))
       (and (grammatical-edit-is-string-node-p current-node)
            (> (point) (tsc-node-start-position current-node))))

     ;; Support *.vue string.
     (and (string-equal (file-name-extension (buffer-file-name)) "vue")
          (nth 3 (grammatical-edit-current-parse-state)))

     (grammatical-edit-before-string-close-quote-p))))

(defun grammatical-edit-in-single-quote-string-p ()
  (ignore-errors
    (let ((parent-node-text (tsc-node-text (tsc-get-parent (tree-sitter-node-at-point)))))
      (and (grammatical-edit-in-string-p)
           (> (length parent-node-text) 1)
           (string-equal (substring parent-node-text 0 1) "'")))))

(defun grammatical-edit-before-string-close-quote-p ()
  (let ((current-node (tree-sitter-node-at-point)))
    (and
     (= (point) (tsc-node-start-position current-node))
     (string-equal (tsc-node-type current-node) "\"")
     (save-excursion
       (forward-char (length (tsc-node-text current-node)))
       (not (grammatical-edit-is-string-node-p (tree-sitter-node-at-point)))
       ))))

(defun grammatical-edit-after-open-quote-p ()
  (and (not (string-equal (grammatical-edit-node-type-at-point) "\""))
       (save-excursion
         (backward-char 1)
         (string-equal (grammatical-edit-node-type-at-point) "\""))))

(defun grammatical-edit-before-string-open-quote-p ()
  (and (not (grammatical-edit-in-string-p))
       (not (grammatical-edit-in-empty-string-p))
       (or (string-equal (grammatical-edit-node-type-at-point) "\"")
           (eq (grammatical-edit-node-type-at-point) 'raw_string_literal))))

(defun grammatical-edit-in-comment-p ()
  (or (eq (grammatical-edit-node-type-at-point) 'comment)
      (and (point-at-eol)
           (ignore-errors
             (save-excursion
               (backward-char 1)
               (eq (grammatical-edit-node-type-at-point) 'comment))))))

(defun grammatical-edit-in-char-p (&optional argument)
  (let ((argument (or argument (point))))
    (and (eq (char-before argument) ?\\ )
         (not (eq (char-before (1- argument)) ?\\ )))))

(defun grammatical-edit-is-blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun grammatical-edit-only-whitespaces-before-cursor-p ()
  (let ((string-before-cursor
         (buffer-substring
          (save-excursion
            (beginning-of-line)
            (point))
          (point))))
    (equal (length (string-trim string-before-cursor)) 0)))

(defun grammatical-edit-newline (arg)
  (interactive "p")
  (cond
   ;; Just newline if in string or comment.
   ((or (grammatical-edit-in-comment-p)
        (grammatical-edit-in-string-p))
    (newline arg))
   ((derived-mode-p 'inferior-emacs-lisp-mode)
    (ielm-return))
   ;; Newline and indent region if cursor in parentheses and character is not blank after cursor.
   ((and (looking-back "(\s*\\|{\s*\\|\\[\s*")
         (looking-at-p "\s*)\\|\s*}\\|\s*\\]"))
    ;; Insert blank below at parentheses.
    (newline arg)
    (open-line 1)
    ;; Indent close parentheses line.
    (save-excursion
      (grammatical-edit-jump-left)
      (grammatical-edit-match-paren 1)
      (indent-according-to-mode))
    ;; Indent blank line.
    (indent-according-to-mode))
   ;; Newline and indent.
   (t
    (newline arg)
    (indent-according-to-mode))))

(defun grammatical-edit-jump-up ()
  (interactive)
  (let* ((current-node (tree-sitter-node-at-point))
         (parent-node (tsc-get-parent current-node)))
    (if parent-node
        (let ((parent-node-start-position (tsc-node-start-position parent-node)))
          (if (equal parent-node-start-position (point))
              (progn
                (backward-char)
                (grammatical-edit-jump-up))
            (goto-char parent-node-start-position)
            (scroll-down 3)))
      (grammatical-edit-jump-left)
      (back-to-indentation))))

;; Integrate with eldoc
(with-eval-after-load 'eldoc
  (eldoc-add-command-completions
   "grammatical-edit-"))

(provide 'grammatical-edit)

;;; grammatical-edit.el ends here
