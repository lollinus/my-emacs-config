;; rc-company.el ---

(ensure-package-installed 'company)
(ensure-package-installed 'company-c-headers)

(require 'company)
(require 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

(provide 'rc-company)

;; rc-company.el ends here
