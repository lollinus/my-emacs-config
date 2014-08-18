;;; rc-python.el ---
;; python mode configuration

(ensure-package-installed 'python-mode)

;; (setq python-python-command "python2.7")
;; (setq auto-mode-alist
;;       (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode)
;;             interpreter-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook 'font-lock-fontify-numbers)
;; (add-hook 'python-mode-hook (lambda () (require 'py-smart-operator)
;;                               (py-smart-operator-mode)))

;;; rc-python.el ends here
