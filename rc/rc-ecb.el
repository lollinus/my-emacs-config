;;; rc-ecb.el ---

(if running-ms-windows
	(if (package-installed-p 'ecb)
		(progn
		  (require 'ecb-autoloads)
		  (require 'ecb)
		  )
	  (print "ecb not installed - skipping autoloads")
	  )
  (progn
	(ensure-package-installed 'ecb)

	;;(require 'ecb-autoloads)
	(require 'ecb)
	)
  )

;;; rc-ecb.el ends here
