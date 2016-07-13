;;; rc-ws-butler.el ---

(ensure-package-installed 'ws-butler)

(add-hook 'c-mode-common-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)

(provide 'rc-ws-butler)

;;; rc-ws-butler.el ends here
