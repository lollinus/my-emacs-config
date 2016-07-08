;;; rc-flymake.el ---

(ensure-package-installed 'flymake-google-cpplint)

(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "~/.emacs.d/scripts/cpplint.py"))
  (flymake-google-cpplint-load)
)

(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;;; rc-flymake.el ends here
