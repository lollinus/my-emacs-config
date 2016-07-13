;; -*- mode: lisp; coding: utf-8-unix -*-
;; Load all configuration parts

(package-initialize)

;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "karol.barski@tieto.com")

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq gc-cons-threshold 100000000)

(global-set-key (kbd "M-;") 'commend-dwim-2)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

(load "~/.emacs.d/rc/environment.el")
;;(load "~/.emacs.d/rc/emacs.0.el")
;;(load "~/.emacs.d/rc/emacs.1.el")

;; elpa configuration -- keep it always first because other configs can try to install packages
;;(load "~/.emacs.d/rc/rc-w3m.el")
(load "~/.emacs.d/rc/rc-elpa.el")
(require 'rc-functions)
(require 'rc-whitespace)

;; individual modes loading
(require 'rc-cua)
;;(load "~/.emacs.d/rc/rc-buffer-move.el")
(require 'rc-yasnippet)
(require 'rc-anzu)

(require 'rc-color-theme)
;;(load "~/.emacs.d/rc/rc-column-marker.el")
(require 'rc-undo-tree)
;;(load "~/.emacs.d/rc/rc-adoc-mode.el")
;;(load "~/.emacs.d/rc/rc-markdown.el")
(require 'rc-doxymacs)
;;(load "~/.emacs.d/rc/rc-makefile-mode.el")
;;(load "~/.emacs.d/rc/rc-cmake-mode.el")

;;(load "~/.emacs.d/rc/rc-auto-complete.el")
;;(load "~/.emacs.d/rc/rc-flymake.el")
;; (load "~/.emacs.d/rc/rc-google-c-style.el")
(require 'rc-iedit)
(require 'rc-duplicate-thing)
(require 'rc-cc-mode)

(require 'rc-diff-mode)
(require 'rc-volatile-highlights)
(require 'rc-clean-aindent-mode)
(require 'rc-dtrt-indent)
(require 'rc-ws-butler)

(require 'rc-global)
(require 'rc-helm)
(require 'rc-helm-gtags)

;; (load "~/.emacs.d/rc/rc-ecb.el")
(require 'rc-cedet)

(require 'rc-company)

(require 'rc-gdb)

(require 'rc-smartparens)
(require 'rc-projectile)
(require 'rc-zygospore)

;;(load "~/.emacs.d/rc/rc-irony-mode.el")
;;(load "~/.emacs.d/rc/rc-java-mode.el")
;;(load "~/.emacs.d/rc/rc-js-mode.el")
;; (load "~/.emacs.d/rc/rc-web-mode.el")

;;(load "~/.emacs.d/rc/rc-graphviz.el")

;; Load ruby only when needed
;; (load "~/.emacs.d/rc/rc-ruby.el")
;;(load "~/.emacs.d/rc/rc-python.el")
(require 'rc-magit)
;;(load "~/.emacs.d/rc/rc-git.el")

;; (load "~/.emacs.d/rc/rc-org-mode.el")
;;(load "~/.emacs.d/rc/rc-org-addons.el")
;(load "~/.emacs.d/rc/rc-gnus.el")

;; (load "~/.emacs.d/rc/rc-alpha.el")

;; (load "~/.emacs.d/rc/rc-haskell-mode.el")
;;(load "~/.emacs.d/rc/rc-auctex.el")

;;(load "~/.emacs.d/rc/rc-psvn.el")

(message "Init finished")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-c-headers zygospore ws-butler volatile-highlights undo-tree smartparens python-mode psvn markdown-mode magit irony iedit helm-systemd helm-swoop helm-projectile helm-gtags helm-gitignore helm-git-grep helm-git-files helm-git helm-c-yasnippet helm-c-moccur graphviz-dot-mode google-c-style git-blame git ggtags flymake-google-cpplint ecb duplicate-thing dtrt-indent company column-marker cmake-font-lock clean-aindent-mode buffer-move anzu adoc-mode ac-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
