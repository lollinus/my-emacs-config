;;; rc-irony-mode.el ---

(ensure-package-installed 'irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in 
;; irony-mode's buffers by irony-mode's function
(defun my:irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my:irony-mode-hook)

;; (defun my:irony-enable()
;;   (when (member major-mode irony-supported-major-modes)
;;     (irony-mode 1)))
;; (add-hook 'c++-mode-hook 'my:irony-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete using irony  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat my-site-lisp-directory "ac-irony"))
(require 'ac-irony)

(defun my-ac-irony-setup ()
  (add-to-list 'ac-sources 'ac-source-irony)
  (auto-complete-mode 1)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

(add-hook 'irony-mode-hook 'my-ac-irony-setup)

;;; rc-irony-mode.el ends herep
