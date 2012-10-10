;;; rc-auctex.el ---
(add-to-list 'load-path
             (concat my-site-lisp-directory "site-start.d"))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)

(setq TeX-PDF-mode t)
(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))

(eval-after-load 'info
  '(add-to-list 'Info-directory-list (concat my-site-lisp-directory "auctex/info")))

(setq preview-image-type 'pnm)

;;; rc-auctex.el ends here
