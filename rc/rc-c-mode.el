;;; rc-c-mode.el ---
;;--------------------------------------------------------------------------------
;; styl indentacji kodu
;;--------------------------------------------------------------------------------
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "linux")
        (other . "bsd")))

;;--------------------------------------------------------------------------------
;; użycie tabulatora
;;--------------------------------------------------------------------------------
(setq default-tab-width 8)
(setq indent-tabs-mode t)
(setq c-basic-offset 8)
;; (require 'cc-mode)
;; ;; personal preferences
;; (c-set-offset 'substatement-open 0)
;; (c-set-offset 'case-label '+)
;; (c-set-offset 'arglist-cont-nonempty '+)
;; (c-set-offset 'arglist-intro '+)
;; (c-set-offset 'topmost-intro-cont '+)

;;; rc-c-mode.el ends here
