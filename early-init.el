;;; early-init --- Early initialization  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Used in emacs 27 to speed up initial package loading
(setq package-quickstart t)
(setq package-enable-at-startup nil)
;; use the system monospace font
(setq-default font-use-system-font t)

(setq load-prefer-newer t)
;; silence native-compile warnings
(setq native-comp-async-report-warnings-errors 'silent)
(provide 'early-init)

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold  most-positive-fixnum)
(setq gc-cons-threshold  (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq garbage-collection-messages t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

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

;;; Bootstrap

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

;;
;;; Set Custom file
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; (setq package-enable-at-startup nil  ; To avoid initializing twice
;;       package-check-signature nil)

(provide 'early-init)
;;; early-init.el ends here
