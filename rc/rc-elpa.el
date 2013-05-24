;; configure melpa package archive
;(add-to-list 'package-archives
;             (cons "melpa" "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))



;; Add the user-contributed repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

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
