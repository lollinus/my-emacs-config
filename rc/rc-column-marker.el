;;; rc-column-marker.el ---
;; configuration of column markers

(ensure-package-installed 'column-marker)

(require 'column-marker)
;; highlight column 80 in cc-mode
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (column-marker-1 80)))

;; use `C-c m' interactively to highlight with `column-marker-1-face'
(global-set-key (kbd "C-c m") 'column-marker-2)

(defface column-marker-4 '((t (:background "orchid4")))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

(defvar column-marker-4-face 'column-marker-4
  "Face used for a column marker.  Usually a background color.
Changing this directly affects only new markers." )

(defface column-marker-5 '((t (:background "orchid5")))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

(defvar column-marker-5-face 'column-marker-5
  "Face used for a column marker.  Usually a background color.
Changing this directly affects only new markers." )

(column-marker-create column-marker-4 column-marker-4-face)
(column-marker-create column-marker-5 column-marker-5-face)

(provide 'rc-column-marker)

;;; rc-column-marker.el ends here
