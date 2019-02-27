;;; rc-color-theme.el ---

(defvar my:terminal-theme 'wombat)
(defvar my:window-theme 'misterioso)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (unless my:theme-window-loaded
                      (if my:theme-window-loaded
                          (enable-theme my:window-theme)
                        (load-theme my:window-theme t))
                      (setq my:theme-window-loaded t))
                  (unless my:theme-terminal-loaded
                    (if my:theme-terminal-loaded
                        (enable-theme my:terminal-theme)
                      (load-theme my:terminal-theme t))
                      (setq my:theme-terminal-loaded t)))))
  (progn
    (if (display-graphic-p)
	(progn 
	  (load-theme my:window-theme t)
	  (setq my:theme-window-loaded t)
	  )
      (progn
	(load-theme my:terminal-theme t)
	(setq my:theme-terminal-loaded t)
        )))
  )

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
