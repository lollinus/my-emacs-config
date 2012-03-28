;;; rc-color-theme.el ---
(if (not (string-equal "24" (substring emacs-version 0 2)))
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
          (load-theme 'tango-dark)
        (load-theme 'wombat))
      )
    )
  "For emacs 24 and above use builtin color-theme library"
  )

(switch-color-theme)
(global-set-key (kbd "C-x c") 'switch-color-theme)


;; rc-color-theme.el ends here
