;;; rc-alpha.el ---  -*- lexical-binding: t; -*-

;;--------------------------------------------------------------------------------
;; transparent emacs window on M$
;;--------------------------------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(defun kb/toggle-transparency ()
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 20))))

(global-set-key (kbd "C-c t") 'kb/toggle-transparency)

;;; rc-alpha.el ends here
