;;; rc-whitespace.el ---

(setq-default whitespace-style
	      '(face
		lines-tail
		newline
		empty
		indentation big-indent
		space-before-tab))

(global-set-key (kbd "C-c w") 'whitespace-mode)

(provide 'rc-whitespace)

;;; rc-whitespace.el ends here
