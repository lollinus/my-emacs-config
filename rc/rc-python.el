;;; rc-python.el ---
;; python mode configuration

(ensure-package-installed 'python-mode)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

;; use the wx backend, for both mayavi and matpplotlib
(setq py-python-command-args
	  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

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
