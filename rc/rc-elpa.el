;;; rc-elpa.el ---
;; configure melpa package archive

(require 'package)

;; Add the user-contributed repository
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every pacxkage is installed, ask for installation if it's not.

Return a list of installed packages or nil ofr every skipped package."
  (mapcar
   (lambda (package)
     ;;(package-installed-p 'evil)
     (if (package-installed-p package)
         nil
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
