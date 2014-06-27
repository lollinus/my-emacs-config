;;; rc-makefile-mode.el

(defun kb:makefile-setup ()
  "Setup makefile mode"
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  )

(add-hook 'makefile-mode-hook 'kb:makefile-setup)

;;; rc-makefile-mode ---
