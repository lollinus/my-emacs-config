;;; rc-adoc-mode.el ---

(add-to-list 'load-path (concat my-site-lisp-directory "adoc-mode"))

(autoload 'adoc-mode "adoc-mode")

(setq auto-mode-alist
      (append '(
                ("\\.adoc$" . adoc-mode)
                ("\\.asciidoc$" . adoc-mode))
              auto-mode-alist
              )
      )

;;; rc-adoc-mode.el ends here
