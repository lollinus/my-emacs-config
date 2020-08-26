;;; rc-color-theme.el --- Color theme config file
;;; Commentary:

;;; Code:
;; font configuration
(defun kb/set-font ()
  "Function set screen font.
If Emacs is run in MS Windows then use Arial Unicode MS
On U*x systems Use DejaVu Sans Mono"
  (if running-ms-windows
      (set-frame-parameter nil 'font "Unifont")
                                        ;(set-frame-parameter nil 'font "Arial Unicode MS")
    (set-frame-parameter nil 'font "DejaVu Sans Mono"))
  )

(kb/set-font)

(defvar kb/terminal-theme 'wombat)
(defvar kb/window-theme 'misterioso)
(defvar kb/theme-window-loaded nil)
(defvar kb/theme-terminal-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
		    (progn
		      (unless kb/theme-window-loaded
			(if kb/theme-window-loaded
			    (enable-theme kb/window-theme)
			  (load-theme kb/window-theme t))
			(setq kb/theme-window-loaded t))
		      (kb/set-font)
		      )
                  (unless kb/theme-terminal-loaded
                    (if kb/theme-terminal-loaded
                        (enable-theme kb/terminal-theme)
                      (load-theme kb/terminal-theme t))
		    (setq kb/theme-terminal-loaded t)))))
  (progn
    (if (display-graphic-p)
	(progn
	  (load-theme kb/window-theme t)
	  (setq kb/theme-window-loaded t)
	  )
      (progn
	(load-theme kb/terminal-theme t)
	(setq kb/theme-terminal-loaded t)
        )))
  )

(provide 'rc-color-theme)
;;; rc-color-theme.el ends here
