;; markdown and markdown+ modes configuration
(add-to-list 'load-path
             (concat my-site-lisp-directory "markdown-mode"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "markdown-mode-plus"))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.mdown$" . markdown-mode) auto-mode-alist))

(require 'markdown-mode+)

;; -*- mode: emacs-lisp -*- 
