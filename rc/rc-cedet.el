;;; rc-cedet.el ---

(require 'cc-mode)
(require 'semantic)

;; load local projects if they exists
;; (if (file-exists-p "~/my-ede-projects.el")
;;     (load "~/my-ede-projects.el")
;;   nil)

; you can use system-include-path for setting up the system header file locaqtions.
; turn on automatic reparsing of open buffers in semantic
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

; turn on Semantic
(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

; lets' define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

; turn on ede mode
(require 'ede)
(global-ede-mode 1)

(provide 'rc-cedet)

;;; rc-cedet.el ends here
