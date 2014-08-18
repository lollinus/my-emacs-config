;;; rc-clearcase.el ---

;;----------------------------------------------------------------------
;; clearcase mode
;;----------------------------------------------------------------------
(if (not running-ms-windows)
	(progn
	  (add-to-list 'load-path
				   (concat my-site-lisp-directory "clearcase"))
	  (require 'clearcase)
	  )
  )

;;; rc-clearcase.el ends here
