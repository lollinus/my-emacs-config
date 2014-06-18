;;; rc-java-mode.el ---

(setq auto-mode-alist
      (append '(
		("\\.js$" . java-mode)
		) auto-mode-alist))
(add-hook 'java-mode-hook (lambda ()
		 (setq show-trailing-whitespace t)))

;; highlight FIXME, TODO and XXX as warning in some major modes
(font-lock-add-keywords 'java-mode '(("\\(XXX\\|FIXME\\|TODO\\)") 1 font-lock-warning-face prepend))

;;; rc-java-mode.el ends here
