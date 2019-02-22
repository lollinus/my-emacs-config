;;; rc-color-theme.el ---
(if (not (or
		  (string-equal "24" (substring emacs-version 0 2))
		  (string-equal "25" (substring emacs-version 0 2))
		  (string-equal "26" (substring emacs-version 0 2))
		  ))
    (progn
      (add-to-list 'load-path
                   (concat my-site-lisp-directory "color-theme-library"))
      (require 'color-theme)
      (color-theme-initialize)
      (defun switch-color-theme ()
        "Switch color theme.
Theme is chosen depending if current window is displayed on TTY or graphical window."
        (interactive "*")
        (if window-system
            (color-theme-subtle-hacker)
          (color-theme-hober))
        )
      )
  (progn
    (defun switch-color-theme ()
      "Switch color theme.
Theme is chosen depending if current window is displayed on TTY or graphical window."
      (interactive "*")
      (if window-system
          (load-theme 'misterioso)
        (load-theme 'wombat))
      )
    )
  "For emacs 24 and above use builtin color-theme library"
  )

(switch-color-theme)
(global-set-key (kbd "C-x c") 'switch-color-theme)

; font configuration
(defun kb-set-font ()
  "Function sets screen font
If emacs is run in MS Windows then use Arial Unicode MS
On U*x systems Use DejaVu Sans Mono
"
  (if running-ms-windows
      (set-frame-parameter nil 'font "Unifont")
                                        ;(set-frame-parameter nil 'font "Arial Unicode MS")
    (set-frame-parameter nil 'font "DejaVu Sans Mono"))
)

(kb-set-font)

(provide 'rc-color-theme)

;; rc-color-theme.el ends here
