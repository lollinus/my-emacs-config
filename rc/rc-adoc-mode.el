;;; rc-adoc-mode.el ---

(add-to-list 'load-path (concat my-site-lisp-directory "adoc-mode"))
(add-to-list 'load-path (concat my-site-lisp-directory "asciidoc-mode"))

(autoload 'adoc-mode "adoc-mode")

(setq auto-mode-alist
      (append '(
                ("\\.adoc$" . doc-mode)
                ("\\.asciidoc$" . doc-mode))
              auto-mode-alist
              )
      )

(add-hook 'doc-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (require 'asciidoc)))

;;; rc-adoc-mode.el ends here
