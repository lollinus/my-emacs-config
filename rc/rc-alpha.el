;;; rc-alpha.el ---

;;--------------------------------------------------------------------------------
;; transparent emacs window on M$
;;--------------------------------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(eval-when-compile (require 'cl))
(defun kb/toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(92 50))))

(global-set-key (kbd "C-c t") 'kb/toggle-transparency)

;;; rc-alpha.el ends here
