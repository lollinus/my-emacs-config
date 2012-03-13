;; rc-color-theme.el
(if (not (string-equal "24" (substring emacs-version 0 2)))
    (progn
      (add-to-list 'load-path
                   (concat my-site-lisp-directory "color-theme-library"))
      (require 'color-theme)
      (color-theme-initialize)
      (if window-system
          (color-theme-subtle-hacker)
        (color-theme-hober))
      )
  (progn
    (if window-system
        (load-theme 'tango-dark)
      (load-theme 'wombat))
    )
  "For emacs 24 and above use builtin color-theme library"
  )
