;;; rc-moccur.el ---

(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
	 :map isearch-mode-map
	 ("M-o" . isearch-moccur)
	 ("M-O" . isearch-moccur-all))
  :init
  (ensure-package-installed 'color-moccur)
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit)
  )

(provide 'rc-moccur)

;;; rc-moccur.el ends here
