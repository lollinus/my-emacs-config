;;; early-init --- Early initialization
;;; Commentary:
;;; Code:
;; Used in emacs 27 to speed up initial package loading
(setq package-quickstart t)
(setq package-enable-at-startup nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; use the system monospace font
(setq-default font-use-system-font t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq load-prefer-newer t)
;; silence native-compile warnings
(setq native-comp-async-report-warnings-errors 'silent)
(provide 'early-init)
;;; early-init.el ends here
