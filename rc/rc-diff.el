;;; rc-diff.el ---
;; additional configurations for diff mode

(add-hook 'diff-mode-hook
		  (lambda ()
			(if (file-exists-p "./scripts/checkpatch.pl")
				(progn (print "setting compile-command")
					   (set (make-local-variable 'compile-command)
							(concat "./scripts/checkpatch.pl --emacs "
									(buffer-file-name))))
			  (print "checkpatch not found")
			  )
			))


;;; rc-diff.el ends here
