;;; rc-duplicate-thing.el ---

(ensure-package-installed 'duplicate-thing)

;; PACKAGE: duplicate-thing
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

(provide 'rc-duplicate-thing)

;;; rc-duplicate-thing.el ends here
