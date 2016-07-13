;;; rc-anzu.el ---

(ensure-package-installed 'anzu)

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'rc-anzu)

;;; rc-anzu.el ends here
