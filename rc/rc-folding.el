;; rc-folding.el
(add-to-list 'load-path
             (concat my-site-lisp-directory "folding.el"))

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
