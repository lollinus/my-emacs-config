;;; rc-iedit.el ---

(message "Loading rc-iedit")

(ensure-package-installed 'iedit)

(require 'iedit)
(global-unset-key (kbd "C-;"))
(define-key global-map (kbd "C-c ;") 'iedit-mode)

(provide 'rc-iedit)

;;; rc-iedit.el ends here
