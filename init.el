;; -*- mode: lisp; coding: utf-8-unix -*-
;; Load all configuration parts

(load "~/.emacs.d/rc/environment.el")
(load "~/.emacs.d/rc/emacs.0.el")
(load "~/.emacs.d/rc/emacs.1.el")

;; elpa configuration -- keep it always first because other configs can try to install packages
(load "~/.emacs.d/rc/rc-elpa.el")

;; individual modes loading
(load "~/.emacs.d/rc/rc-cua.el")
(load "~/.emacs.d/rc/rc-buffer-move.el")
(load "~/.emacs.d/rc/rc-yasnippet.el")
(load "~/.emacs.d/rc/rc-color-theme.el")
(load "~/.emacs.d/rc/rc-column-marker.el")
(load "~/.emacs.d/rc/rc-undo-tree.el")
(load "~/.emacs.d/rc/rc-folding.el")
(load "~/.emacs.d/rc/rc-adoc-mode.el")
(load "~/.emacs.d/rc/rc-doxymacs.el")
(load "~/.emacs.d/rc/rc-makefile-mode.el")

(load "~/.emacs.d/rc/rc-auto-complete.el")
;; (load "~/.emacs.d/rc/rc-flymake.el")
;; (load "~/.emacs.d/rc/rc-google-c-style.el")
(load "~/.emacs.d/rc/rc-iedit.el")
;; (load "~/.emacs.d/rc/rc-cedet.el")
(load "~/.emacs.d/rc/rc-c-mode.el")
(load "~/.emacs.d/rc/rc-irony-mode.el")
(load "~/.emacs.d/rc/rc-java-mode.el")

;; Load ruby only when needed
;;(load "~/.emacs.d/rc/rc-ruby.el")
(load "~/.emacs.d/rc/rc-python.el")
(load "~/.emacs.d/rc/rc-magit.el")
(load "~/.emacs.d/rc/rc-nxhtml.el")

;; (load "~/.emacs.d/rc/rc-org-mode.el")
;(load "~/.emacs.d/rc/rc-gnus.el")

(load "~/.emacs.d/rc/rc-alpha.el")

;(load "~/.emacs.d/rc/rc-haskell-mode.el")
(load "~/.emacs.d/rc/rc-auctex.el")

(load "~/.emacs.d/rc/rc-psvn.el")
