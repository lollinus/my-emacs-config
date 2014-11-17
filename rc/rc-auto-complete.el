;;; rc-auto-complete.el ---

(ensure-package-installed 'auto-complete)

(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

; let's define a function which initializes auto-complete-c-headers and gets called for C/C++ hooks
(ensure-package-installed 'ac-c-headers)
(defun my:ac-c-header-init ()
  (require 'ac-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
  )

; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;; rc-auto-complete.el ends here ---
