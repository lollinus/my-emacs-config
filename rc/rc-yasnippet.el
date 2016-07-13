;;; rc-yasnippet.el ---

(ensure-package-installed 'yasnippet)

(require 'yasnippet)

;; (eval-after-load 'yasnippet
;;   '(progn
;;     (add-to-list 'yas/snippet-dirs "~/rc/snippets")
;;     (yas/load-snippet-dirs)))

;; hook for automatic reloading of changed snippets
;; (defun alexott/update-yasnippets-on-save ()
;;   (when (string-match "/yasnippet/snippets" buffer-file-name)
;;     (yas/load-snippet-dirs)
;; ;;    (yas/reload-all)
;;     ))
;; (add-hook 'after-save-hook 'alexott/update-yasnippets-on-save)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/rc/snippets")
(yas/global-mode 1)
(setq yas-prompt-functions (quote (yas-ido-prompt yas-completing-prompt yas-no-prompt)))

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

(provide 'rc-yasnippet)

;;; rc-yasnippet.el ends here
