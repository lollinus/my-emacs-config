;; rc-undo-tree.el
(add-to-list 'load-path
             (concat my-site-lisp-directory "undo-tree"))

(require 'undo-tree)
(global-undo-tree-mode)
