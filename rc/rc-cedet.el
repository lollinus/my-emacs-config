;;; rc-cedet.el ---

; turn on Semantic
(semantic-mode 1)

; lets' define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

; turn on ede mode
(global-ede-mode 1)

;; load local projects if they exists
(if (file-exists-p "~/my-ede-projects.el")
    (load "~/my-ede-projects.el")
  nil)

; you can use system-include-path for setting up the system header file locaqtions.
; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

;;; rc-cedet.el ends here
