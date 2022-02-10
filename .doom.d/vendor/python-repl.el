;;; python-repl.el --- A minor mode for a Python REPL -*- lexical-binding:t; no-byte-compile:t -*-

;; Copyright (C) 2021 Francesco Giannelli
;; Author: Francesco Giannelli <francesco.giannelli@yahoo.it>
;; Keywords: languages
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1")(s "1.12"))
;; URL: https://github.com/untoreh/python-repl

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-python-repl")
;; (require 'python-repl)

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; Run a python REPL inside a terminal in Emacs. Use
;; the Python REPL facilities for interactive features, such readline,
;; help, debugging.

;;; Code:

(require 'term)
(require 'cl-generic)
(require 'cl-lib)
(require 'compile)
(require 's)
(require 'seq)
(require 'subr-x)


;;
;; customizations
;;

(defgroup python-repl nil
  "A minor mode for a Python REPL"
  :group 'python)

(defcustom python-repl-inferior-buffer-name-base "python"
  "Prefix for the names for inferior REPL buffers.

See ‘python-repl--inferior-buffer-name’."
  :type 'string
  :group 'python-repl)

(defcustom python-repl-hook nil
  "Hook to run after starting a Python REPL term buffer."
  :type 'hook
  :group 'python-repl)

(defcustom python-repl-captures (list (kbd "M-x"))
  "List of key sequences that are passed through (the global binding is used).

Note that this affects all buffers using the ‘ansi-term’ map."
  :type '(repeat key-sequence)
  :group 'python-repl)

(defcustom python-repl-compilation-mode t
  "When non-nil, highlight error locations using function ‘compilation-shell-minor-mode’."
  :type 'boolean
  :group 'python-repl)

(defcustom python-repl-save-buffer-on-send nil
  "When non-nil, save buffer without prompting on send."
  :type 'boolean
  :group 'python-repl)

(defcustom python-repl-path-rewrite-rules nil
  "A list of rewrite rules applied to paths sent with ‘include’ and similar.

Each rule should be a function that is called with a path, in the given order. If the function returns ‘nil’, the next one is tried, otherwise the result is used as the rewritten path.

This can be used a workaround, usually necessary on non-Unix systems. Eg for Cygwin-based Windows, use

   (setq python-repl-path-rewrite-rules python-repl-cygwin-path-rewrite-rules)

in your Emacs init file after loading this package."
  :type 'list
  :group 'python-repl)

(defcustom python-repl-path-cygwin-prefix "c:/cygwin64"
  "Prepended to paths by some Cygwin rewrite rules when no other information is available."
  :type 'string
  :group 'python-repl)

(defcustom python-repl-set-term-escape t
  "Set the escape char C-x globally for term. Useful for switching windows, but users who do not want this globally should set it to nil."
  :type 'boolean
  :group 'python-repl)

;;;; utility functions

(defun python-repl--add-earmuffs (buffer-name)
  "Add earmuffs (*'s) to BUFFER-NAME.

This matches the buffer name (eg created by ‘make-term’)."
  (concat "*" buffer-name "*"))

;;;; terminal backends

;;; generic api

(cl-defgeneric python-repl--locate-live-buffer (terminal-backend name)
  "Return the inferior buffer with NAME if it has a running REPL, otherwise NIL.")

(cl-defgeneric python-repl--make-buffer (terminal-backend name executable-path switches)
  "Make and return a new inferior buffer.

Buffer will be named with NAME (earmuffs added by this function), starting python using EXECUTABLE-PATH with SWITCHES (a list of strings).")

(cl-defgeneric python-repl--send-to-backend (terminal-backend buffer string paste-p ret-p)
  "Send a string to BUFFER using the given backend.

When PASTE-P, “bracketed paste” mode will be used. When RET-P, terminate with an extra newline.")

;;;; ansi-term

(cl-defstruct python-repl--buffer-ansi-term
  "Terminal backend via ‘ansi-term’, available in Emacs.")

(cl-defmethod python-repl--locate-live-buffer ((_terminal-backend python-repl--buffer-ansi-term)
                                               name)
  (if-let ((inferior-buffer (get-buffer (python-repl--add-earmuffs name))))
      (with-current-buffer inferior-buffer
        (cl-assert (eq major-mode 'term-mode) nil "Expected term-mode. Changed mode or backends?"))
    (when (term-check-proc inferior-buffer)
      inferior-buffer)))

(cl-defmethod python-repl--make-buffer ((_terminal-backend python-repl--buffer-ansi-term)
                                        name executable-path switches)
  (let ((inferior-buffer (apply #'make-term name executable-path nil switches)))
    (with-current-buffer inferior-buffer
      (mapc (lambda (k)
              (define-key term-raw-map k (global-key-binding k)))
            python-repl-captures)
      (term-char-mode)
      (when python-repl-set-term-escape
        (term-set-escape-char ?\C-x))      ; useful for switching windows
      (setq-local term-prompt-regexp "^(python|shell|help\\?|(\\d+\\|debug ))>")
      (when (version< emacs-version "27")
        (setq-local term-suppress-hard-newline t))  ; reflow text
      (setq-local term-scroll-show-maximum-output t)
      ;; do I need this?
      (setq-local term-scroll-to-bottom-on-output t))
    inferior-buffer))

(cl-defmethod python-repl--send-to-backend ((_terminal-backend python-repl--buffer-ansi-term)
                                            buffer string paste-p ret-p)
  "Send a string to BUFFER using the given backend.

When PASTE-P, “bracketed paste” mode will be used. When RET-P, terminate with an extra newline."
  (with-current-buffer buffer
    (when paste-p                       ; bracketed paste start
      (term-send-raw-string "\e[200~"))
    (term-send-raw-string string)
    (when ret-p                         ; return
      (term-send-raw-string "\^M"))
    (when paste-p                       ; bracketed paste stop
      (term-send-raw-string "\e[201~"))))

;;; vterm

(with-eval-after-load 'vterm

  (defun python-repl--next-error-function (n &optional reset)
    "A workaround for https://github.com/akermu/emacs-libvterm/issues/316."
    ;; NOTE remove when that issue is fixed
    (interactive "p")
    (goto-char (point))
    (compilation-next-error-function n reset))

  (defun python-repl--has-running-vterm-process (inferior-buffer)
    "Return non-nil if ‘inferior-buffer’ has a running vterm process."
    (let ((proc (buffer-local-value 'vterm--process inferior-buffer)))
      (and proc (memq (process-status proc) '(run stop open listen connect)))))

  (cl-defstruct python-repl--buffer-vterm
    "Terminal backend using ‘vterm’, which needs to be installed and loaded.")

  (cl-defmethod python-repl--locate-live-buffer ((_terminal-backend python-repl--buffer-vterm)
                                                 name)
    (if-let ((inferior-buffer (get-buffer (python-repl--add-earmuffs name))))
        (with-current-buffer inferior-buffer
          (cl-assert (eq major-mode 'vterm-mode) nil "Expected vterm-mode. Changed mode or backends?")
          ;; cf https://github.com/akermu/emacs-libvterm/issues/270
          (when (python-repl--has-running-vterm-process inferior-buffer)
            inferior-buffer))))

  (cl-defmethod python-repl--make-buffer ((_terminal-backend python-repl--buffer-vterm)
                                          name executable-path switches)
    (let ((vterm-buffer (get-buffer-create (python-repl--add-earmuffs name)))
          (inhibit-read-only t))
      (with-current-buffer vterm-buffer
        (let ((vterm-shell (s-join " " (cons executable-path switches))))
          (vterm-mode)
          (local-set-key (kbd "C-c C-z") #'python-repl--switch-back)
          ;; NOTE workaround for https://github.com/akermu/emacs-libvterm/issues/316, remove when fixed
          (add-hook 'compilation-shell-minor-mode-hook
                    ;; NOTE run *after* vterm's hook and overwrite `next-error-function'
                    (lambda () (setq next-error-function 'python-repl--next-error-function))
                    t t)))
      vterm-buffer))

  (cl-defmethod python-repl--send-to-backend ((_terminal-backend python-repl--buffer-vterm)
                                              buffer string paste-p ret-p)
    (with-current-buffer buffer
      (vterm-send-string string paste-p)
      (when ret-p
        (vterm-send-return)))))

;;
;; global variables
;;

(defconst python-repl--rx-at
  (rx (seq "@" (syntax whitespace)
           (? (group (+ alnum)) space)  ; package name
           (group (+ (not (any space ">" "<" "(" ")" "\t" "\n" "," "'" "\"" ";" ":")))) ; path
           ":" (group (+ num))))        ; line
  "Matches “@ Foo ~/code/Foo/src/Foo.jl:100”")

(defvar python-repl--compilation-regexp-alist
  `(;; matches "while loading /tmp/Foo.jl, in expression starting on line 2"
    (python-load-error . ("while loading \\([^ ><()\t\n,'\";:]+\\), in expression starting on line \\([0-9]+\\)" 1 2))
    ;; matches "around /tmp/Foo.jl:2", also starting with "at" or "Revise"
    (python-loc . ("\\(around\\|at\\|Revise\\) \\([^ ><()\t\n,'\";:]+\\):\\([0-9]+\\)" 2 3))
    ;; matches "omitting file /tmp/Foo.jl due to parsing error near line 2", from Revise.parse_source!
    (python-warn-revise . ("omitting file \\([^ ><()\t\n,'\";:]+\\) due to parsing error near line \\([0-9]+\\)" 1 2))
    (python-error-at . (,python-repl--rx-at 2 3))
    )
  "Specifications for highlighting error locations.

Uses function ‘compilation-shell-minor-mode’.")

(defvar python-repl--terminal-backend
  (make-python-repl--buffer-ansi-term)
  "Terminal backend, for internal use. Set using `python-repl-set-terminal-backend'.")

(defun python-repl-set-terminal-backend (backend)
  "Set terminal backend for `python-repl'.

Valid backends are currently:

- ‘ansi-term’, using the ANSI terminal built into Emacs.

- ‘vterm’, which requires that vterm is installed. See URL ‘https://github.com/akermu/emacs-libvterm’."
  (interactive "S")
  (cl-case backend
    ('ansi-term
     (setq python-repl--terminal-backend (make-python-repl--buffer-ansi-term)))
    ('vterm
     (require 'vterm)
     (setq python-repl--terminal-backend (make-python-repl--buffer-vterm)))
    (otherwise
     (error "Unrecognized backend “%s”." backend))))

(defconst python-repl-executable-records
  `((default ,(file-name-nondirectory (my/select-first #'executable-find '("ptipython" "ptpython" "ipython" "python")))))
  "List of Python executables.

Entries have the form

  (KEY EXECUTABLE-PATH :BASEDIR BASEDIR)

A missing :BASEDIR will be completed automatically when first used.

This is used for key lookup for ‘python-repl-executable-key’. The
first entry is the default.")

(defun python-repl--default-executable-key ()
  "Return the default executable key."
  (let ((key (caar python-repl-executable-records)))
    (cl-assert key nil "Could not find any key in PYTHON-REPL-EXECUTABLE-RECORDS.")
    key))

(defvar python-repl-inferior-buffer-name-suffix nil
  "Name for the Python REPL buffer associated with a source code buffer.

Can be a symbol (with NIL being the default) or a number. See
‘python-repl--inferior-buffer-name’ for details on how it is
used to generate a buffer name.")

(defvar-local python-repl--inferior-buffer-suffix nil
  "Suffix for a specific inferior buffer.

These are used for offering choices when selecting new suffix.
For internal use only.")

(defvar-local python-repl--script-buffer nil
  "Buffer active before calling `python-repl'.")

(defvar python-repl-executable-key nil
  "Key for the executable associated with the buffer.

Looked up in ‘python-repl-executable-records’. When nil, the
first value is used.

See ‘python-repl--inferior-buffer-name’ for how it is used to
generate a buffer name.")

(defun python-repl--get-executable-key ()
  "Return the executable key, picking the first one if it was not set."
  (or python-repl-executable-key (python-repl--default-executable-key)))

(defvar python-repl-switches nil
  "Command line switches for the Python executable.

Valid values are NIL or a string. These take effect the next time
a new Python process is started.")

;;
;; REPL buffer creation and setup
;;

(cl-defun python-repl--inferior-buffer-name
    (&optional (executable-key (python-repl--get-executable-key))
               (suffix python-repl-inferior-buffer-name-suffix))
  "Name for a Python REPL inferior buffer.

The name is a string, constructed from PYTHON-REPL-INFERIOR-NAME-BASE and EXECUTABLE-KEY (used only when different from the global default), and the SUFFIX.

An integer SUFFIX is formatted as “<SUFFIX>”, while a symbol is added as “-SUFFIX.”

Note that ‘make-term’ surrounds this string by *'s when converted to a buffer name. See ‘python-repl--add-earmuffs’."
  (let* ((middle (if (eq executable-key (python-repl--default-executable-key))
                     ""
                   (format "-%s" executable-key)))
         (last (cond
                ((null suffix) "")
                ((integerp suffix) (format "<%d>" suffix))
                ((symbolp suffix) (format "-%s" suffix))
                (t (error
                    "Inferior name suffix should be an integer or a symbol")))))
    (concat python-repl-inferior-buffer-name-base middle last)))


(cl-defun python-repl--capture-basedir (executable-path)
  "Attempt to obtain the Python base directory by querying the Python executable.

When NIL, this was unsuccessful."
  "~/.pythonble")

(defun python-repl--complete-executable-record! (executable-record)
  "Complete EXECUTABLE-RECORD if necessary.

Queries and appends missing information if necessary.

Note: when cannot capture the base dir, it is set to NIL to
prevent further attempts."
  (unless (plist-member (cddr executable-record) :basedir)
    (let* ((executable-path (cl-second executable-record))
           (basedir (python-repl--capture-basedir executable-path)))
      (nconc executable-record `(:basedir ,basedir))
      (unless basedir
        (warn "could not capture basedir for Python executable %s"
              executable-path)))))

(defun python-repl--setup-compilation-mode (inferior-buffer basedir)
  "Setup compilation mode for the the current buffer in INFERIOR-BUFFER.

BASEDIR is used for resolving relative paths."
  (with-current-buffer inferior-buffer
    (setq-local compilation-error-regexp-alist-alist
                python-repl--compilation-regexp-alist)
    (setq-local compilation-error-regexp-alist
                (mapcar #'car compilation-error-regexp-alist-alist))
    (when basedir
      (setq-local compilation-search-path (list basedir)))
    (compilation-shell-minor-mode 1)))

(defun python-repl--run-hooks (inferior-buffer)
  "Run the hooks in ‘python-repl-hook’ in INFERIOR-BUFFER."
  (with-current-buffer inferior-buffer
    (run-hooks 'python-repl-hook)))

(cl-defun python-repl--executable-record (executable-key)
  "Return the executable record for EXECUTABLE-KEY.

It is looked up in ‘python-repl-executable-records’. An error is
raised if not found."
  (let ((executable-record (assq executable-key python-repl-executable-records)))
    (unless executable-record
      (error "Could not find %s in PYTHON-REPL-EXECUTABLE-RECORDS"
             executable-key))
    executable-record))

;;
;; prompting for executable-key and suffix
;;

(defun python-repl--matching-inferior-buffers (executable-key)
  "A list of macthing inferior buffers for the current source buffer.

Matches the EXECUTABLE-KEY, without the suffix."
  (let* ((inferior-buffer-name-root
          (python-repl--inferior-buffer-name executable-key nil))
         (inferior-buffer-name-regexp (concat "\\*"
                                              inferior-buffer-name-root
                                              ".*\\*")))
    (seq-filter (lambda (buffer)
                  (string-match inferior-buffer-name-regexp
                                (buffer-name buffer)))
                (buffer-list))))

(defun python-repl--read-inferior-buffer-name-suffix (executable-key)
  "Completing read of a inferior buffer name suffix for the Python REPL.

Returns a symbol, from interning the string that is read. The
completions offered are specific to the EXECUTABLE-KEY.

See ‘python-repl--inferior-buffer-name’."
  (let* ((matching-inferior-buffers
          (python-repl--matching-inferior-buffers executable-key))
         (suffix-buffer-alist (mapcar
                               (lambda (buffer)
                                 (cons (buffer-local-value
                                        'python-repl--inferior-buffer-suffix
                                        buffer)
                                       buffer))
                               matching-inferior-buffers))
         (suffix-buffer-alist (cl-stable-sort suffix-buffer-alist
                                              (lambda (x y)
                                                (or (not x)
                                                    (string< (prin1-to-string x)
                                                             (prin1-to-string y))))
                                              :key #'car))
         (suffix (completing-read "python-repl inferior buffer name suffix: "
                                  suffix-buffer-alist)))
    (message "suffix buffer alist %s" suffix)
    (intern suffix)))

(cl-defun python-repl--unused-inferior-buffer-name-index (executable-key)
  "First positive integer that is not used as an inferior buffer name suffix.

See ‘python-repl--inferior-buffer-name’."
  (let ((index 1))
    (while (get-buffer (python-repl--add-earmuffs
                        (python-repl--inferior-buffer-name executable-key
                                                           index)))
      (cl-incf index))
    index))

(defun python-repl-prompt-set-inferior-buffer-name-suffix (arg)
  "Prompt for and set a Python REPL inferior buffer name for the current buffer.

A prefix argument ARG modifies the behavior:

- \\[negative-argument] selects the next unused number for the suffix (ie a new
buffer),
- an numerical prefix selects that integer for the suffix.

Both of these happen without prompting."
  (interactive "P")
  (let* ((executable-key (python-repl--get-executable-key))
         (suffix (cond
                  ((null arg)
                   (python-repl--read-inferior-buffer-name-suffix executable-key))
                  ((eq arg '-)
                   (python-repl--unused-inferior-buffer-name-index executable-key))
                  ((integerp arg)
                   arg)
                  ((listp arg)
                   (cl-first arg)))))
    (setq python-repl-inferior-buffer-name-suffix suffix)
    (message "python-repl-inferior-buffer-name-suffix set to %s" suffix)))

(defun python-repl-prompt-switches ()
  "Read and set the switches for the inferior process."
  (interactive)
  (let ((switches (read-string "switches for the python process: " python-repl-switches)))
    (message "python-repl-switches set to \"%s\"" switches)
    (setq python-repl-switches switches)))

(defun python-repl-prompt-executable-key ()
  "Prompt for the key of the Python REPL executable.

Valid keys are the first items in ‘python-repl-executable-records’."
  (intern
   (completing-read "python-repl executable: "
                    python-repl-executable-records nil t nil)))

(defun python-repl-prompt-set-executable-key ()
  "Prompt and save the key of the Python REPL executable.

Valid keys are the first items in ‘python-repl-executable-records’."
  (interactive)
  (let ((key (python-repl-prompt-executable-key)))
    (setq python-repl-executable-key key)
    (message "python-repl-executable-key set to %s"
             (propertize (symbol-name key) 'face 'font-lock-constant-face))))

;;
;; high-level functions
;;

(cl-defun python-repl-inferior-buffer (&key (executable-key (python-repl--get-executable-key))
                                            (suffix python-repl-inferior-buffer-name-suffix)
                                            (terminal-backend python-repl--terminal-backend ))
  "Return the Python REPL inferior buffer, creating one if it does not exist."
  (let* ((name (python-repl--inferior-buffer-name executable-key suffix))
         (live-buffer (python-repl--locate-live-buffer terminal-backend name)))
    (if live-buffer
        live-buffer
      (let ((executable-record (python-repl--executable-record executable-key))
            (switches python-repl-switches))
        (python-repl--complete-executable-record! executable-record)
        (let* ((executable-path (cl-second executable-record))
               (basedir (plist-get (cddr executable-record) :basedir))
               (inferior-buffer (python-repl--make-buffer terminal-backend name executable-path
                                                          (when switches
                                                            (split-string switches)))))
          (when python-repl-compilation-mode
            (python-repl--setup-compilation-mode inferior-buffer basedir))
          (python-repl--run-hooks inferior-buffer)
          (setf (buffer-local-value 'python-repl--inferior-buffer-suffix inferior-buffer) suffix)
          inferior-buffer)))))

;;;###autoload
(defun python-repl ()
  "Raise the Python REPL inferior buffer, creating one if it does not exist.

This is the standard entry point for using this package."
  (interactive)
  (let ((script-buffer (current-buffer))
	(inferior-buffer (python-repl-inferior-buffer)))
    (with-current-buffer inferior-buffer
      (setq python-repl--script-buffer script-buffer))
    (pop-to-buffer inferior-buffer)))

(defun python-repl--switch-back ()
  "Switch to the buffer that was active before last call to `python-repl'."
  (interactive)
  (when (buffer-live-p python-repl--script-buffer)
    (switch-to-buffer-other-window python-repl--script-buffer)))

;;
;; path rewrites
;;

(defun python-repl--path-rewrite (path rules)
  "Call each rule (function) in ‘rules’ with ‘path’. When the
result is non-nil, return that and terminate, when all rules are
tested return ‘path’ unchanged."
  (let ((result nil))
    (while (and (not result) rules)
      (setf result (funcall (car rules) path)
            rules (cdr rules)))
    (if result
        result
      path)))

(defun python-repl--cygwin-replace-cygdrive (path)
  "Rewrite ‘/cygdrive/c/something’ to ‘c:/something’."
  (let ((m (s-match-strings-all "^/cygdrive/\\([A-Za-z]\\)\\(/.*\\)$" path)))
    (when m
      (let ((m1 (cl-first m)))
        (s-concat (cl-second m1) ":" (cl-third m1))))))

(defun python-repl--cygwin-add-drive (path)
  "When the path does not start with a Windows drive letter,
prepend ‘python-repl-path-cygwin-prefix’."
  (unless (s-matches? "^[A-Za-z]:/" path)
    (s-concat python-repl-path-cygwin-prefix path)))

(defconst python-repl-cygwin-path-rewrite-rules
  (list #'python-repl--cygwin-replace-cygdrive
        #'python-repl--cygwin-add-drive)
  "Default list of rewrite rules for Cygwin. Use as a starting
  point, you may need to copy and modify this. See
  ‘python-repl-path-cygwin-prefix’.")

;;
;; sending to the REPL
;;

(defun python-repl--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Python REPL term buffer.

The string is trimmed, then a closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to ‘current-prefix-arg’,
  3. otherwise no newline.

Unless NO-BRACKETED-PASTE, bracketed paste control sequences are used."
  (when (eq no-newline 'prefix)
    (setq no-newline current-prefix-arg))
  (let ((inferior-buffer (python-repl-inferior-buffer)))
    (display-buffer inferior-buffer)
    (python-repl--send-to-backend python-repl--terminal-backend
                                  inferior-buffer (s-trim string) (not no-bracketed-paste)
                                  (not no-newline))))

(defun python-repl-send-line ()
  "Send the current line to the Python REPL term buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `python-repl-send-region-or-line'."
  (interactive)
  (python-repl--send-string (thing-at-point 'line t) 'prefix t)
  (forward-line))

(defun python-repl-send-region-or-line (&optional prefix suffix)
  "Send active region (if any) or current line to the inferior buffer.

Closed with a newline, unless used with a prefix argument.

When PREFIX and SUFFIX are given, they are concatenated before
and after."
  (interactive)
  (cl-flet ((-send-string (string)
                          (python-repl--send-string
                           (concat prefix string suffix) 'prefix)))
    (if (use-region-p)
        (progn
          (-send-string (buffer-substring-no-properties
                         (region-beginning) (region-end)))
          (deactivate-mark))
      (progn
        (-send-string (thing-at-point 'line t))
        (forward-line)))))

(defun python-repl-edit ()
  "Call @edit on the expression.

Selection semantics same as ‘python-repl-send-region-or-line’."
  (interactive)
  (python-repl-send-region-or-line "@edit "))

(defun python-repl-macroexpand ()
  "Expand the expression as a macro.

Selection semantics same as ‘python-repl-send-region-or-line’."
  (interactive)
  (python-repl-send-region-or-line "macroexpand(Main, quote " " end)"))

(defun python-repl-send-buffer (arg)
  "Send the contents of the current buffer to the Python REPL.

Use ‘include’ by default if the buffer is associated with a file,
and is not modified (ie has been saved) or saved after
prompting.  Otherwise send the contents directly; you can force
this with a prefix argument ARG."
  (interactive "P")
  (let* ((file (and (not arg) buffer-file-name)))
    (when (and file (buffer-modified-p))
      (if (or python-repl-save-buffer-on-send (y-or-n-p "Buffer modified, save? "))
          (save-buffer)
        (setq file nil)))
    (python-repl--send-string
     (if file
         (concat "execfile(\""
                 (python-repl--path-rewrite file python-repl-path-rewrite-rules)
                 "\");")
       (buffer-substring-no-properties (point-min) (point-max))))))


(defun python-repl-includet-buffer ()
  "Attempts to include a buffer via ‘Revise.includet’.

If a buffer does not correspond to a file, the function does nothing, just shows a message.

If a buffer corresponds to a file and is not saved, the function prompts the user to save.  If the user declines to save an unsaved file, the command is still run if the file otherwise exists, in which case ‘Revise’ will load it later when saved."
  (interactive)
  (let* ((file buffer-file-name))
    (if file
        (progn
          (when (buffer-modified-p)
            (if (or python-repl-save-buffer-on-send (y-or-n-p "Buffer modified, save? "))
                (save-buffer)
              (unless (file-exists-p file)
                (message "need to save the file first"))))
          (python-repl--send-string (concat "Revise.includet(\""
                                            (python-repl--path-rewrite file python-repl-path-rewrite-rules)
                                            "\");")))
      (message "buffer does not correspond to a file"))))

(defun python-repl--symbols-after-dot ()
  "Dot-separated symbols after point (which should be on a dot), as a list."
  (when (eq (char-after) ?.)
    (forward-char)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (cl-destructuring-bind (start1 . end1) bounds
          (let ((symbol1 (buffer-substring-no-properties start1 end1)))
            (goto-char (1+ end1))
            (cons symbol1 (python-repl--symbols-after-dot))))))))

(defun python-repl--symbols-before-dot ()
  "Dot-separated symbols before point (which should be on a dot), as a list, in reverse order."
  (when (eq (char-after) ?.)
    (backward-char)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (cl-destructuring-bind (start1 . end1) bounds
          (let ((symbol1 (buffer-substring-no-properties start1 end1)))
            (goto-char (1- start1))
            (cons symbol1 (python-repl--symbols-before-dot))))))))

(defun python-repl--symbols-at-point ()
  "Return the a list of symbols at point (eg a variable, function, or module
name), separated by dots, as a list."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (cl-destructuring-bind (start1 . end1) bounds
        (let ((symbol1 (buffer-substring-no-properties start1 end1))
              (symbols-before-dot (save-excursion
                                    (goto-char (1- start1))
                                    (python-repl--symbols-before-dot)))
              (symbols-after-dot (save-excursion
                                   (goto-char end1)
                                   (python-repl--symbols-after-dot))))
          (cl-concatenate 'list (nreverse symbols-before-dot) (list symbol1) symbols-after-dot))))))

(defun python-repl-doc ()
  "Documentation for symbol at point."
  (interactive)
  (python-repl--send-string (concat "@doc " (s-join "." (python-repl--symbols-at-point)))))

(defun python-repl-list-methods ()
  "List methods for symbol at point."
  (interactive)
  (python-repl--send-string (concat "methods(" (s-join "." (python-repl--symbols-at-point)) ")")))

(defun python-repl-cd (&optional directory)
  "Change directory to the directory of the current buffer (if applicable)."
  (interactive)
  (if-let* ((dir (or directory buffer-file-name default-directory))
            (cddir (file-name-directory dir)))
      (progn
	(python-repl--send-string (concat "import os; os.chdir(\""
                                          (python-repl--path-rewrite cddir python-repl-path-rewrite-rules)
                                          "\")"))
	(with-current-buffer (python-repl-inferior-buffer) (cd cddir)))
    (warn "buffer not associated with a file")))

(setq pyvenv-default-virtual-env-name nil
      python-repl-venv-dir ".venv")

(defun python-repl-current-project-dir ()
  (catch 'proj-dir
    (mapc (lambda (file)
            (if-let ((bufname (or buffer-file-name default-directory))
                     (projfile (locate-dominating-file bufname file)))
                (throw 'proj-dir
                       (file-name-directory (file-truename projfile)))))
          `(".projectile"
            "requirements.txt"
            ,python-repl-venv-dir))
    nil))

(defun python-repl-activate-parent (arg)
  "Look for a project file in the parent directories, if found, activate the project.

When called with a prefix argument, activate the home project."
  (interactive "P")
  (if arg
      (progn
        (message "activating home project")
        (pyvenv-activate arg)
        (python-repl-cd))
    (progn
      (if-let* ((venv-dir python-repl-venv-dir)
                (proj-dir (python-repl-current-project-dir))
                (proj-venv-dir (expand-file-name venv-dir proj-dir))
                (src-dir (my/concat-path proj-dir "src")))
          (progn
            (message "activating %s" proj-venv-dir)
            (cd proj-dir)
            (ignore-errors (pyvenv-activate proj-venv-dir))
            (python-repl--send-string
             (concat "%cd " (if (file-directory-p src-dir)
                                src-dir proj-dir))))
        (message "could not find project file")))))

(defun python-repl-set-python-editor (editor)
  "Set the PYTHON_EDITOR environment variable."
  (interactive)
  (python-repl--send-string (format "ENV[\"PYTHON_EDITOR\"] = \"%s\";" editor)))

(defun python-repl-use-emacsclient ()
  "Use emacsclient as the PYTHON_EDITOR."
  (interactive)
  (python-repl--send-string "ENV[\"PYTHON_EDITOR\"] = \"emacsclient\";"))

(defun python-repl-live-buffer ()
  (let* ((executable-key (python-repl--get-executable-key))
         (suffix python-repl-inferior-buffer-name-suffix)
         (terminal-backend python-repl--terminal-backend)
         (name (python-repl--inferior-buffer-name executable-key suffix))
         (live-buffer (python-repl--locate-live-buffer terminal-backend name)))
    live-buffer))

(defun python-repl-switch (&optional no-activate cd)
  " Enables python repl, and activates the current project "
  (if (not (fboundp #'python-repl-inferior-buffer))
      (require 'python-repl))
  ;; we query for the buffer before checking for skip because we
  ;; want to switch buffer anyway
  (let ((startup (not (python-repl-live-buffer))))
    (if (python-repl-inferior-buffer)
        (progn
          (if (and startup (not no-activate))
              (progn
                (python-repl-cd (projectile-project-root))
                (when (member (car (alist-get 'default python-repl-executable-records))
                              '("jupyter" "jupyter-console" "ipython"))
                  (python-repl--send-string "\n%load_ext autoreload \n%autoreload 2\n"))
                (ignore-errors
                  (python-repl-activate-parent nil)))
            (progn
              (when cd
                (python-repl-cd (projectile-project-root)))
              (python-repl)))
          t)
      nil)))

(defun python-repl-toggle-debug ()
  " Toggle the debug python logging level (10) from the default (30).

The function is defined in ipython startup file found with `get_ipython().profile_dir.startup_dir'
Default is '~/.ipython/profile_default/startup/..."
  (interactive)
  (python-repl-cmd "toggle_debug();"))

(defun python-repl-cmd (str)
  "Send a string to python repl switching to its buffer, if it exists."
  (when (python-repl-switch nil t)
    (python-repl--send-string str)))

;;;###autoload
(define-minor-mode python-repl-mode
  "Minor mode for interacting with a Python REPL running inside a term.

\\{python-repl-mode-map}"
  nil ">"
  `((,(kbd "C-c C-a")    . python-repl-activate-parent)
    (,(kbd "C-c C-b")    . python-repl-send-buffer)
    (,(kbd "C-c C-c")    . python-repl-send-region-or-line)
    (,(kbd "C-c C-d")    . python-repl-doc)
    (,(kbd "C-c C-e")    . python-repl-edit)
    (,(kbd "C-c C-l")    . python-repl-list-methods)
    (,(kbd "C-c C-m")    . python-repl-macroexpand)
    (,(kbd "C-c C-p")    . python-repl-cd)
    (,(kbd "C-c C-s")    . python-repl-prompt-set-inferior-buffer-name-suffix)
    (,(kbd "C-c C-t")    . python-repl-includet-buffer)
    (,(kbd "C-c C-v")    . python-repl-prompt-set-executable-key)
    (,(kbd "C-c C-z")    . python-repl)
    (,(kbd "<C-return>") . python-repl-send-line))
  (when-let ((filename (buffer-file-name)))
    (setq-local default-directory (file-name-directory filename))))

(provide 'python-repl)
;;; python-repl.el ends here
