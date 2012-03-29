;;; rc-python.el ---
;; python mode configuration
(setq python-python-command "python2.6")
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook 'font-lock-fontify-numbers)

;;; rc-python.el ends here
