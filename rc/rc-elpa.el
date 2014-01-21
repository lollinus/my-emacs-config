;; configure melpa package archive

(require 'package)

;; Add the user-contributed repository
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

;; (progn
;;   (switch-to-buffer
;;    (url-retrieve-synchronously
;;     "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
;;   (package-install-from-buffer (package-buffer-info) 'single))

;;(defadvice package-compute-transaction
;;  (before package-compute-transaction-reverse (package-list requirements) activate compile)
;;  "reverse the requirements"
;;  (setq requirements (reverse requirements))
;;  (print (requirements)))

(package-initialize)
