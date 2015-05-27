;;; rc-org-mode.el
(add-to-list 'load-path
             (concat my-site-lisp-directory "org-mode/lisp"))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
;; iswitchb is obsolete
;;(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-ditaa-jar-path "~/java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")
(setq org-src-fontify-natively t)
(setq org-alphabetical-lists t)
(setq org-time-stamp-custom-formats (cons "<%d-%m-%y>" "<%d-%m-%y %H:%M>"))
(setq org-display-custom-times t)

;; tables should always used fixed width font
(set-face-attribute 'org-table nil :family "courier")
(set-face-attribute 'org-column nil :family "courier")

;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(org-babel-do-load-languages 'org-babel-load-languages '(
                                                         (emacs-lisp . t)
                                                         (awk . t)
                                                         (sh . t)
                                                         (C . t)
                                                         (sql . t)
                                                         (latex . t)
                                                         (org . t)
                                                         (dot . t)
                                                         (python . t)
                                                         )
                             )

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq org-export-htmlize-output-type 'css)
;;             (local-set-key (quote [?\C-c ?\C-x]) 'org-export-body-as-html)))

;; (org-babel-do-load-languages
;;  (quote org-babel-load-languages)
;;  (quote ((emacs-lisp . t)
;;          (dot . t)
;;          (ditaa . t)
;;          (R . t)
;;          (python . t)
;;          (ruby . t)
;;          (gnuplot . t)
;;          (clojure . t)
;;          (sh . t)
;;          (ledger . t)
;;          (org . t)
;;          (plantuml . t)
;;          (latex . t))))


;;--------------------------------------------------------------------------------
;; outline-mode
;;--------------------------------------------------------------------------------
;;(add-to-list 'auto-mode-alist
;;  '("\\.list\\'" . outline-mode))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
;; (setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
;; (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;; rc-org-mode.el end here ---
