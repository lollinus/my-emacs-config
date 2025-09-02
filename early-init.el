;;; early-init --- Early initialization  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Faster to disable these here (before they've been initialized)
(setq default-frame-alist
	  (append
	   '((menu-bar-lines . 0)
		 (tool-bar-lines . 0)
		 (vertical-scroll-bars)
		 (fullscreen . maximized))
	   (when (featurep 'ns)
		 '((ns-transparent-titlebar . t)))
	   default-frame-alist))
(setq-default custom-file (concat user-emacs-directory "custom.el"))

(provide 'early-init)
;;; early-init.el ends here
