


(when (package-installed-p 'haskell-mode)
  (use-package haskell-mode)
  (use-package lsp-haskell :ensure t
    :hook
    (haskell-mode . lsp)
    (haskell-literate-mode . lsp)
    )
  (use-package flycheck-haskell :ensure t)
  (use-package company-ghc
    :if (package-installed-p 'company)
    :ensure t
    :config
    (add-to-list 'company-backends 'company-ghc)
    )
  )

(when (package-installed-p 'tuareg)
  (use-package tuareg
    :after (smartparens)
    :config
    (defun kb/tuareg-prettify-symbols ()
      (if (functionp 'prettify-symbols-mode)
          (prettify-symbols-mode)))
    :hook (tuareg-mode . kb/tuareg-prettify-symbols)
    )
  (use-package flycheck-ocaml :ensure t)
  (use-package merlin
    :config
    (if (boundp 'company-box-backends-colors)
	(add-to-list 'company-box-backends-colors
		     '(merlin-company-backend . (:all "light blue"
						      :candidate (:background "white" :foreground "black")
    						      :annotation (:background "blue" :foreground "green")
    						      :selected (:background "yellow" :foreground "dark blue")))))
    :hook (tuareg-mode . merlin-mode))
  )

(when (package-installed-p 'auctex)
  (use-package auctex
    :if (package-installed-p 'auctex)
    :no-require t
    :custom
    (TeX-PDF-mode t)
    (TeX-view-program-selection
     '((output-dvi "DVI Viewer")
       (output-pdf "PDF Viewer")
       (output-html "HTML Viewer")))
    (preview-image-type 'pnm)
    :config
    (message "AucTeX configuration"))
  )

;; (use-package ispell
;;   :config
;;   (when (executable-find "enchant-2")
;;     (setq ispell-program-name "enchant-2")))

;;(use-package rust-mode
;;  :ensure t
;;  )
;;(use-package cargo
;;  :ensure t
;;  )
;;(use-package rustic
;;  :ensure t
;;  )

