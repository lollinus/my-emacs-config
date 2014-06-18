;;; rc-buffer-move.el ---

(ensure-package-installed 'buffer-move)

(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; override org-mode keyboard configuration
(add-hook 'org-mode-hook '(lambda ()
                            (local-set-key "<C-S-up>" 'buf-move-up)
                            (local-set-key "<C-S-down>" 'buf-move-down)
                            (local-set-key "<C-S-left>" 'buf-move-left)
                            (local-set-key "<C-S-right>" 'buf-move-right)))



;;; rc-buffer-move.el ends here ---
