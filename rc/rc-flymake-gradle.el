;;; rc-flymake-gradle.el ---

(ensure-package-installed 'flymake-gradle)

(use-package flymake-gradle
	     :ensure t
	     :commands (flymake-gradle-add-hook)
	     :init
	     (add-hook 'java-mode-hook #'flymake-gradle-add-hook)
	     (add-hook 'kotlin-mode-hook #'flymake-gradle-add-hook)
	     )


;;; rc-flymake-gradle.el ends here
