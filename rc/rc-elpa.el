;;; rc-elpa.el ---
;; configure melpa package archive

(if (version< emacs-version "24")
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
	(when (load (expand-file-name "~/.emacs.d/package.el"))
	  (package-initialize)))
  "Emacs >= 24 has elpa integrated")

(require 'package)

(message "package-archives: %s" package-archives)
(set 'package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/"))))
(message "package-archives: %s" package-archives)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections, 
which is unsafe because it allows man-in-the middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this  warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(message "package-archives: %s" package-archives)

;; Add the user-contributed repository
;; (add-to-list 'package-archives (cons "gnu" "http://elpa.gnu.org/packages/"))
;;(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/"))
;;(add-to-list 'package-archives (cons "marmalade" "https://marmalade-repo.org/packages/"))
 

(setq package-archive-priorities
      '(
;;	("melpa" . 0)
;;	("melpa-mirror" . 1)
	("gnu" . 2)
;;	("marmalade". 3)
	)
      )

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

(package-initialize)

;;; rc-elpa.el ends here
