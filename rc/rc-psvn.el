;;; rc-psvn.el ---
;; svn mode configuration

(ensure-package-installed 'psvn)

(require 'psvn)
(setq svn-user-names-including-blanks '("Karol Barski"
                                        "Zbigniew Zagorski")) ; username on severs containing blanks
(add-hook 'svn-pre-parse-status-hook 'svn-status-parse-fixup-user-names-including-blanks)

