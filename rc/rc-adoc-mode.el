;;; rc-adoc-mode.el ---

(ensure-package-installed 'adoc-mode)

;; (add-to-list 'load-path (concat my-site-lisp-directory "adoc-mode"))
;; (add-to-list 'load-path (concat my-site-lisp-directory "asciidoc-el"))
;; (autoload 'adoc-mode "adoc-mode")

(setq auto-mode-alist
      (append '(
                ("\\.adoc$" . adoc-mode)
                ("\\.asciidoc$" . adoc-mode))
              auto-mode-alist
              )
      )

;; (add-hook 'adoc-mode-hook
;;           '(lambda ()
;;              (turn-on-auto-fill)
;;              (require 'asciidoc)))

;;; rc-adoc-mode.el ends here
