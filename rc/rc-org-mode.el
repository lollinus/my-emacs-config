;;; rc-org-mode.el
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-ditaa-jar-path "~/java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")
(setq org-src-fontify-natively t)
(setq org-alphabetical-lists t)
(setq org-time-stamp-custom-formats (cons "<%d-%m-%y>" "<%d-%m-%y %H:%M>"))
(setq org-display-custom-times t)

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


;;; rc-org-mode.el end here ---
