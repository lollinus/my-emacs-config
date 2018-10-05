;;; rc-magit-gerrit.el ---

(ensure-package-installed 'magit-gerrit)

(require 'magit-gerrit)

;;(require 'magit)
(setq-default magit-gerrit-ssh-creds "kbarskix@git-amr-3.devtools.intel.com")

(provide 'rc-magit-gerrit)

;;; rc-magit.el ends here
