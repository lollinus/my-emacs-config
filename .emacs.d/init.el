;; -*- mode: lisp; coding: utf-8-unix -*-
;; Load all configuration parts

(load "~/rc/environment.el")
(load "~/rc/emacs.0.el")
(load "~/rc/emacs.1.el")

;; individual modes loading
(load "~/rc/rc-buffer-move.el")
(load "~/rc/rc-elpa.el")
(load "~/rc/rc-markdown-mode.el")
(load "~/rc/rc-yasnippet.el")
(load "~/rc/rc-color-theme.el")
(load "~/rc/rc-column-marker.el")
(load "~/rc/rc-undo-tree.el")
(load "~/rc/rc-folding.el")
(load "~/rc/rc-adoc-mode.el")
(load "~/rc/rc-doxymacs.el")

;; Load ruby only when needed
;;(load "~/rc/rc-ruby.el")
(load "~/rc/rc-python.el")
(load "~/rc/rc-magit.el")
(load "~/rc/rc-nxhtml.el")

(load "~/rc/rc-org-mode.el")
(load "~/rc/rc-calfw.el")
(load "~/rc/rc-gnus.el")

(load "~/rc/rc-alpha.el")
