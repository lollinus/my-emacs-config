;;; rc-dtrt-indent.el ---

(ensure-package-installed 'dtrt-indent)
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 1)

(provide 'rc-dtrt-indent)

;;; rc-dtrt-indent.el ends here
