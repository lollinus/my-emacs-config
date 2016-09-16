;;; rc-cmake-mode.el ---

(ensure-package-installed 'cmake-mode)
(require 'rc-column-marker)

;; (add-to-list 'load-path (concat my-site-lisp-directory "cmake-mode"))

(setq auto-mode-alist
      (append '(
                ("CMakeLists\\.txt\\'" . cmake-mode))
              auto-mode-alist
              )
      )

(add-hook 'cmake-mode-hook
		  (lambda ()
			(setq fill-column 80)
			;;(column-marker-3 80)
			(auto-fill-mode)
			(setq cmake-tab-width 4)
			(setq indent-tabs-mode nil)
			))

;; (autoload 'cmake-mode "cmake-mode")

;;; rc-cmake-mode.el ends here

