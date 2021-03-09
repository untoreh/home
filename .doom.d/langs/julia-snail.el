;;; ../../../var/home/fra/.doom.d/langs/julia-snail.el -*- lexical-binding: t; -*-

(defun julia-snail--query-server (query sym)
  (let* ((module (julia-snail--module-at-point))
         (name (s-concat (s-join "." module) "." sym))
         (res (julia-snail--send-to-server
               :Main
               (format query name)
               :display-error-buffer-on-failure? nil
               :async nil)))
    res))

(defun julia-snail--company-doc-buffer (str)
  (let ((doc (julia-snail--query-server "@doc %s" str)))
    (let ((buf (julia-snail--message-buffer
                julia-snail-repl-buffer
                "doc-buffer"
                (if (eq :nothing doc)
                    "Documentation not found!\nDouble-check your package activation and imports."
                  doc)
                :markdown nil)))
      (with-current-buffer buf
        (font-lock-ensure))
      buf)))

(defun julia-snail--company-annotation (str)
  (let ((res (julia-snail--query-server "methods(%s).ms[1].sig" str)))
    (if (eq res :nothing)
        nil
      res)))

(defun julia-snail-company-capf ()
  (interactive)
  (let* ((comp (julia-snail-repl-completion-at-point))
         (doc (list :company-doc-buffer
                    #'julia-snail--company-doc-buffer))
         (ann (list :annotation-function
                    #'julia-snail--company-annotation)))
    (concatenate 'list comp doc ann)))
