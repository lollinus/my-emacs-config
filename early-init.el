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

;; Some packages (e.g. context-clues) put `;;;###autoload' on a
;; `transient-define-prefix' form, copying it verbatim into their autoloads
;; file.  Built-in transient does not autoload that macro, so evaluating
;; those autoloads during `package-activate-all' fails with void-function.
(autoload 'transient-define-prefix "transient" nil nil 'macro)

(if (not (version< emacs-version "31.1"))
    (setopt user-lisp-directory (expand-file-name "rc" user-emacs-directory))
  )

(provide 'early-init)
;;; early-init.el ends here
