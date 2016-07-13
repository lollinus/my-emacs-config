;;; rc-doxymacs.el ---
;; doxygen mode configuration file

;;(require 'doxygen)
;;(setq doxymacs-doxygen-dirs "d:/Karol/Programy/doxygen/bin")

(add-to-list 'load-path
             (concat my-site-lisp-directory "doxymacs"))
(require 'doxymacs)
(setq doxymacs-use-external-xml-parser t)
(if running-ms-windows
    (progn
      (setq doxymacs-external-xml-parser-executable "xmllint.exe")
      )
  (setq doxymacs-external-xml-parser-executable "xmllint")
  )
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "JavaDoc")

(provide 'rc-doxymacs)

;;; rc-doxymacs.el ends here
