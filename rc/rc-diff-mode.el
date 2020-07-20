;;; rc-diff-mode.el ---

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook
	  (lambda ()
	    (setq-local whitespace-style
			'(face
			  tabs
			  tab-mark
			  spaces
			  space-mark
			  trailing
			  indentation::space
			  indentation::tab
			  newline
			  newline-mark))
	    (whitespace-mode 1)
	    (if (file-exists-p "./scripts/checkpatch.pl")
		(progn (print "setting compile-command")
		       (set (make-local-variable 'compile-command)
			    (concat "./scripts/checkpatch.pl --emacs "
				    (buffer-file-name))))
	      (print "checkpatch not found"))))

(provide 'rc-diff-mode)

;;; rc-diff-mode.el ends here
