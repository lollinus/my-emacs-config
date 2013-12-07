;;; rc-psvn.el ---
;; svn mode configuration
(add-to-list 'load-path
             (concat my-site-lisp-directory "psvn"))

(require 'psvn)
(setq svn-user-names-including-blanks '("Karol Barski"
                                        "Zbigniew Zagorski")) ; username used on SAL9000 contains blanks
(add-hook 'svn-pre-parse-status-hook 'svn-status-parse-fixup-user-names-including-blanks)

