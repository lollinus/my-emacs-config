;; -*- mode: lisp; coding: utf-8-unix -*-
;; Load all configuration parts

(package-initialize)

;; don't let Customize mess with my .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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
;;--------------------------------------------------------------------------------
;;(load "~/.emacs.d/rc/emacs.0.el")
;;--------------------------------------------------------------------------------

(message "The Minibuffer...")

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;;--------------------------------------------------------------------------------
;; zezwalaj na użycie poniższych komend
;;--------------------------------------------------------------------------------
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;--------------------------------------------------------------------------------
(unless window-system (menu-bar-mode 0)) ; turn menus off
(tool-bar-mode 0)			; disable toolbar

;;--------------------------------------------------------------------------------
;; My customized emacs
;;--------------------------------------------------------------------------------
;; scroll bar on the right side
(if window-system
    (set-scroll-bar-mode 'right)
  )

;; fancy streching cursor
(setq x-stretch-cursor t)
;;(global-hl-line-mode t)

;; show column number in mode-line
(column-number-mode t)
(line-number-mode t)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)
(setq kill-whole-line t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format "Emacs: %b %+%+ %f ")

;;--------------------------------------------------------------------------------
;; zegarek
;;--------------------------------------------------------------------------------
(setq display-time-format "%H:%M %d/%m/%Y")
(setq display-time-24hr-format t)
(display-time)

;;--------------------------------------------------------------------------------
;; kolorowanie składni
;;--------------------------------------------------------------------------------
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
;; Set lazy-lock mode (fontifies only when not typing) with .3 sec refresh
;; time and no minimum buffer size
;; Lazy lock gives problems with Java files in RHEL4
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-continuity-time 0.3)
(setq lazy-lock-minimum-size (* 1024 10))       ; Fontify small buffers
(setq font-lock-maximum-size nil)       ; Fontify huge buffers
(setq font-lock-maximum-decoration t)
;; highlight non-breaking spaces
;; (GNUEmacs
;;     (require 'disp-table)
;;     (aset standard-display-table
;;           (make-char 'latin-iso8859-1 (- ?\240 128))
;;           (vector (+ ?\267 (* 524288 (face-id 'nobreak-space))))))

;; highlight FIXME, TODO and XXX as warning in some major modes
(dolist (mode '(c-mode
		cperl-mode
		html-mode-hook
		css-mode-hook
		emacs-lisp-mode))
  (font-lock-add-keywords mode
			  '(("\\(XXX\\|FIXME\\|TODO\\)"
			     1 font-lock-warning-face prepend))))


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
(require 'rc-column-marker)
(require 'rc-undo-tree)
;;(load "~/.emacs.d/rc/rc-adoc-mode.el")
;;(load "~/.emacs.d/rc/rc-markdown.el")
(require 'rc-doxymacs)
;;(load "~/.emacs.d/rc/rc-makefile-mode.el")
;;(load "~/.emacs.d/rc/rc-cmake-mode.el")

(require 'rc-auto-complete)
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

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(require 'rc-ggtags)
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

;; (load "~/.emacs.d/rc/rc-psvn.el")
(message "Init finished")
