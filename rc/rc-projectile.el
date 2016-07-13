;;; rc-projectile.el ---

(ensure-package-installed 'projectile)
(ensure-package-installed 'helm-projectile)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

(provide 'rc-projectile)

;;; rc-projectile.el ends here
