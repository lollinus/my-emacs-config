;;; rc-tuareg.el ---

(add-hook 'tuareg-mode-hook
	  (lambda()
	    (when (functionp 'prettify-symbols-mode)
	      (prettify-symbols-mode))))


(provide 'rc-tuareg)
;;; rc-tuareg.el ends here
