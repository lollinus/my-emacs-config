;;; rc-yasnippet.el ---

;; Copyright (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path
             (concat my-site-lisp-directory "yasnippet"))
(require 'yasnippet)

;; (eval-after-load 'yasnippet
;;   '(progn
;;     (add-to-list 'yas/snippet-dirs "~/rc/snippets")
;;     (yas/load-snippet-dirs)))

;; hook for automatic reloading of changed snippets
;; (defun alexott/update-yasnippets-on-save ()
;;   (when (string-match "/yasnippet/snippets" buffer-file-name)
;;     (yas/load-snippet-dirs)
;; ;;    (yas/reload-all)
;;     ))
;; (add-hook 'after-save-hook 'alexott/update-yasnippets-on-save)

(add-to-list 'yas/snippet-dirs "~/rc/snippets")
(yas/global-mode 1)

;;; rc-yasnippet.el ends here

