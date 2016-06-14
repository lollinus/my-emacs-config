;;; rc-elpa.el ---
;; configure melpa package archive

(if (or
	 (string-equal "24" (substring emacs-version 0 2))
	 (string-equal "25" (substring emacs-version 0 2))
	 )
	"Emacs >= 24 has elpa integrated"
  ;; install elpa on emacs 23
  (progn
	;; (let ((buffer (url-retrieve-synchronously
	;;   	     "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el")))
	;;   (save-excursion
	;;     (set-buffer buffer)
	;;     (goto-char (point-min))
	;;     (re-search-forward "^$" nil 'move)
	;;     (eval-region (point) (point-max))
	;;     (kill-buffer (current-buffer))))

	(when
		(load
		 (expand-file-name "~/.emacs.d/package.el"))
	  (package-initialize))
	)
)

(require 'package)

;; Add the user-contributed repository
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
;;        ("marmalade" . "https://marmalade-repo.org/packages/")
		))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil ofr every skipped package."
  (mapcar
   (lambda (package)
     ;;(package-installed-p 'evil)
     (if (package-installed-p package)
         package
       (package-install package)))
;;       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;;           (package-install package)
;;         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;;(ensure-package-installed 'iedit 'magit) ; --> (nil nil) if iedit and magit are already installed

;; activate installed packages
;; (package-initialize)

(package-initialize)
;;; rc-elpa.el ends here
