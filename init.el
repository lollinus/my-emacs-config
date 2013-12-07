;; -*- mode: lisp; coding: utf-8-unix -*-
;; Load all configuration parts

(load "~/.emacs.d/rc/environment.el")
(load "~/.emacs.d/rc/emacs.0.el")
(load "~/.emacs.d/rc/emacs.1.el")

;; individual modes loading
(load "~/.emacs.d/rc/rc-buffer-move.el")
(load "~/.emacs.d/rc/rc-elpa.el")
(load "~/.emacs.d/rc/rc-yasnippet.el")
(load "~/.emacs.d/rc/rc-color-theme.el")
(load "~/.emacs.d/rc/rc-column-marker.el")
(load "~/.emacs.d/rc/rc-undo-tree.el")
(load "~/.emacs.d/rc/rc-folding.el")
(load "~/.emacs.d/rc/rc-adoc-mode.el")
(load "~/.emacs.d/rc/rc-doxymacs.el")

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
