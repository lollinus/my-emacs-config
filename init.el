;; -*- mode: emacs-lisp; coding: utf-8-unix -*-
;; Load all configuration parts

;; don't let Customize mess with my .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "karol.barski@tieto.com")

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq gc-cons-threshold 100000000)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )
(global-set-key (kbd "RET") 'newline-and-indent)


;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

(load "~/.emacs.d/rc/environment.el")
;;--------------------------------------------------------------------------------
;;(load "~/.emacs.d/rc/emacs.0.el")
;;--------------------------------------------------------------------------------

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;;--------------------------------------------------------------------------------
;; zezwalaj na użycie poniższych komend
;;--------------------------------------------------------------------------------
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;--------------------------------------------------------------------------------
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (progn
                      (tool-bar-mode 0)            ; turn menus off
                      (menu-bar-mode 0)            ; disable toolbar
                      (set-scroll-bar-mode 'right) ; scroll bar on the right side
                      )
                  )))
  (if (display-graphic-p)
      (progn
        (tool-bar-mode 0)               ; turn menus off
        (menu-bar-mode 0)               ; disable toolbar
        (set-scroll-bar-mode 'right)    ; scroll bar on the right side
        )
    )
  )


;;--------------------------------------------------------------------------------
;; My customized emacs
;;--------------------------------------------------------------------------------
;; fancy streching cursor
(setq x-stretch-cursor t)
;; (global-hl-line-mode t)

;; show column number in mode-line
(column-number-mode t)
(line-number-mode t)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)
(setq kill-whole-line t)
;; remap C-H to backspace
(normal-erase-is-backspace-mode t)
(add-hook 'terminal-init-xterm-hook
          (lambda () (normal-erase-is-backspace-mode t))
          )

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format "Emacs: %b %+%+ %f ")

;;--------------------------------------------------------------------------------
;; zegarek
;;--------------------------------------------------------------------------------
(setq display-time-format "%H:%M %d/%m/%Y")
(setq display-time-24hr-format t)
(display-time)

;;--------------------------------------------------------------------------------
;; kolorowanie składni
;;--------------------------------------------------------------------------------
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
;; Set lazy-lock mode (fontifies only when not typing) with .3 sec refresh
;; time and no minimum buffer size
;; Lazy lock gives problems with Java files in RHEL4
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-continuity-time 0.3)
(setq lazy-lock-minimum-size (* 1024 10))       ; Fontify small buffers
(setq font-lock-maximum-size nil)       ; Fontify huge buffers
(setq font-lock-maximum-decoration t)
;; highlight non-breaking spaces
;; (GNUEmacs
;;     (require 'disp-table)
;;     (aset standard-display-table
;;           (make-char 'latin-iso8859-1 (- ?\240 128))
;;           (vector (+ ?\267 (* 524288 (face-id 'nobreak-space))))))

;; highlight FIXME, TODO and XXX as warning in some major modes
(dolist (mode '(c-mode
                cperl-mode
                html-mode-hook
                css-mode-hook
                emacs-lisp-mode))
  (font-lock-add-keywords mode
                          '(("\\(XXX\\|FIXME\\|TODO\\)"
                             1 font-lock-warning-face prepend))))

(setq smerge-command-prefix "\C-cv")

;; elpa configuration -- keep it always first because other configs can try to install packages
;;(load "~/.emacs.d/rc/rc-w3m.el")
(load "~/.emacs.d/rc/rc-elpa.el")

(ensure-package-installed 'use-package)
(setq use-package-compute-statistics t)
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-check-before-init t)

(use-package emacs
  :hook (lisp-mode-hook . display-line-numbers-mode)
  :custom (display-line-numbers-type 'visual)
  )

(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

(use-package comment-dwim-2
  :ensure t
  :config
  (require 'comment-dwim-2)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  )

(require 'rc-functions)
(use-package whitespace
  :custom
  (whitespace-style '(face lines-tail newline empty indentation
                           big-indent space-before-tab))
  :bind
  ("C-c w" . whitespace-mode)
  )

;; individual modes loading
(require 'rc-cua)

;;(load "~/.emacs.d/rc/rc-buffer-move.el")
(use-package yasnippet
  :if (package-installed-p 'yasnippet)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/rc/snippets")
  (yas/global-mode 1)
  ;; Inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))
  :custom
  (yas-prompt-functions '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
  (yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  :hook (term-mode-hook . (lambda() (setq yas-dont-activate t)))
  :bind
  (:map yas-keymap
        ("<return>" . yas/exit-all-snippets)
        ("C-e" . yas/goto-end-of-active-field)
        ("C-a" . yas/goto-start-of-active-field)
        )
  )
;; (require 'rc-anzu)

(use-package smartparens
  :ensure t
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol nil)
  :config
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  )

;; (use-package color-moccur
;;   :ensure t
;;   :commands (isearch-moccur isearch-all)
;;   :bind (("M-s O" . moccur)
;;          :map isearch-mode-map
;;          ("M-o" . isearch-moccur)
;;          ("M-O" . isearch-moccur-all))
;;   :init
;;   (setq isearch-lazy-highlight t)
;;   :config
;;   (use-package moccur-edit
;;     :load-path my-site-lisp-directory)
;;   )

(require 'rc-color-theme)

(use-package fill-column-indicator
  :ensure t
  :config
  (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))
  (defvar sanityinc/fci-mode-suppressed nil)
  (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))

  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))
  )
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))
;;(load "~/.emacs.d/rc/rc-adoc-mode.el")
(use-package markdown-mode
  :ensure t)
;; (require 'rc-doxymacs)
(use-package highlight-doxygen
  :if (package-installed-p 'highlight-doxygen)
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (highlight-doxygen-mode))
            )
  )

;;(load "~/.emacs.d/rc/rc-makefile-mode.el")
;;(load "~/.emacs.d/rc/rc-cmake-mode.el")

;; (require 'rc-auto-complete)
;;(load "~/.emacs.d/rc/rc-flymake.el")
;; (load "~/.emacs.d/rc/rc-google-c-style.el")
(use-package iedit
  :bind ("C-c ;" . iedit-mode)
  )
;; (require 'rc-duplicate-thing)
(require 'rc-cc-mode)

;; (require 'rc-diff-mode)
(use-package volatile-highlights
  :config (volatile-highlights-mode t))
(use-package clean-aindent-mode
  :ensure t
  :config (electric-indent-mode -1)
  :custom (clean-aindent-is-simple-indent t)
  :bind (:map global-map ("<RET>" . newline-and-indent))
  :hook (prog-mode-hook . clean-aindent-mode)
  )

;; (require 'rc-dtrt-indent)
;; (require 'rc-ws-butler)
(use-package ws-butler
  :ensure t
  :hook ((c-mode-common-hook text-mode fundamental-mode) . ws-butler-mode)
  )

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
;;(setq helm-gtags-prefix-key "\C-cg")

;; (require 'rc-ggtags)
(use-package ggtags
  :ensure t
  :hook ((c++-mode-hook c-mode-hook java-mode asm-mode) . (ggtags-mode))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)
              )
  )

;;(require 'rc-helm)
;;(require 'rc-helm-gtags)

(use-package all-the-icons
  :ensure t
  )
(use-package neotree
  :ensure t
  :bind
  ([f8] . neotree-toggle)
  :requires (all-the-icons)
  :after (all-the-icons)
  :config
  ;; needs package all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook
            (lambda (&rest _) (display-line-numbers-mode -1)))

  ;; Every time when the neotree window is opened, let if find current
  ;; file and jump to node.
  (setq neo-smart-open t)

  ;; track 'projectile-switch-project' (C-c p p),
  (setq projectile-switch-project-action 'neotree-projectile-aciton)
  )


(use-package hydra
  :ensure t)
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t)
(use-package ivy
  :ensure t
  :after (swiper counsel)
  :bind (("\C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
  :custom
  (ivy-use-virutal-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

;; (load "~/.emacs.d/rc/rc-ecb.el")
;; (require 'rc-cedet)
;; (require 'rc-company)
;; (require 'rc-gdb)

;; (require 'rc-projectile)
(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :custom (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  ;; (projectile-completion-system 'helm)
  (projectile-enable-caching t)
  :config (projectile-global-mode)
  ;; (helm-projectile-on)
  )
(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;;(load "~/.emacs.d/rc/rc-irony-mode.el")
;; (load "~/.emacs.d/rc/rc-web-mode.el")
;;(load "~/.emacs.d/rc/rc-graphviz.el")

(use-package groovy-mode :ensure t)
;; Load ruby only when needed
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  (py-shell-name "ipython")
  (py-which-bufname "IPython")
  (py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  (py-force-py-shell-name-p t)
  (py-shell-switch-buffers-on-execute-p t)
  (py-switch-buffers-on-execute-p t)
  (py-split-windows-on-execute-p nil)
  (py-smart-indentation t)
  :hook ((python-mode-hook . font-lock-fontify-numbers))
  )
(use-package magit
  :ensure t)
(use-package magit-popup
  :if (package-installed-p 'magit-popup)
  :disabled
  :ensure t)
(use-package magit-gerrit
  :ensure t
  :disabled
  :requires magit-popup
  :custom
  (magit-gerrit-ssh-creds "kbarskix@git-amr-3.devtools.intel.com"))

;; (load "~/.emacs.d/rc/rc-org-mode.el")
;;(load "~/.emacs.d/rc/rc-org-addons.el")
;(load "~/.emacs.d/rc/rc-gnus.el")

;; (load "~/.emacs.d/rc/rc-alpha.el")

;; (load "~/.emacs.d/rc/rc-haskell-mode.el")
;;(load "~/.emacs.d/rc/rc-auctex.el")

;; (load "~/.emacs.d/rc/rc-psvn.el")

(use-package tuareg
  :if (package-installed-p 'tuareg)
  :after (smartparens)
  :hook (tuareg-mode-hook . (lambda ()
                            (when (functionp 'prettify-symbols-mode)
                              (prettify-symbols-mode))))
  )
(message "Init finished")
