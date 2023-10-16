;;; missing-fonts.el -*- lexical-binding: t; -*-

(defvar required-fonts '("Hack.*" "iA Writer Duo S" "Barlow" "Fantasque Sans Mono" "JuliaMono"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     (unless noninteractive
       (add-hook! 'doom-init-ui-hook
         (run-at-time nil nil
                      (lambda ()
                        (message "%s missing the following fonts: %s"
                                 (propertize "Warning!" 'face '(bold warning))
                                 (mapconcat (lambda (font)
                                              (propertize font 'face 'font-lock-variable-name-face))
                                            missing-fonts
                                            ", "))
                        (sleep-for 0.5))))))
  ";; No missing fonts detected")
