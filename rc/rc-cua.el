;;; rc-cua.el ---

;;--------------------------------------------------------------------------------
;; CUA mode configuration
;;--------------------------------------------------------------------------------
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only t) ;; no transient mark mode
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(if (string-equal "21" (substring emacs-version 0 2))
   (print "No fancy CUA stuff sorry")
  (cua-mode 'emacs)
  )

;;; rc-cua.el ends here
