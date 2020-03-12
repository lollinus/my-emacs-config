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

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

(load "~/.emacs.d/rc/environment.el")

(setq vc-follow-symlinks t)
;; blink screen on bell
(setq visible-bell t)

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
;; turn off blinking cursor
;;--------------------------------------------------------------------------------
(blink-cursor-mode -1)

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

(setq load-prefer-newer t)

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
  (font-lock-add-keywords
   mode
   '(("\\<\\(\\(TODO|XXX\\)\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
     ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
     ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(setq smerge-command-prefix "\C-cv")

;; elpa configuration -- keep it always first because other configs can try to install packages
;;(load "~/.emacs.d/rc/rc-w3m.el")
;; (load "~/.emacs.d/rc/rc-elpa.el")

(if (version< emacs-version "24")
    ;; install elpa on emacs 23
    (progn
      ;; (let ((buffer (url-retrieve-synchronously
	;;   	     "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el")))
	;;   (save-excursion
	;;     (set-buffer buffer)
	;;     (goto-char (point-min))
	;;     (re-search-forward "^$" nil 'move)
	;;     (eval-region (point) (point-max))
	;;     (kill-buffer (current-buffer))))
	(when (load (expand-file-name "~/.emacs.d/package.el"))
	  (package-initialize)))
  "Emacs >= 24 has elpa integrated")

(package-initialize)
(setq package-check-signature nil)
(require 'package)


;;(add-to-list 'package-archives
;;	     '("gnu" . "http://elpa.gnu.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (version< emacs-version "24")
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Bootstrap `use-package`
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(setq use-package-compute-statistics t)
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-check-before-init t)

;; (use-package diminish
;;   :ensure t
;;   :config
;;   (diminish 'whitespace-mode)
;;   (eval-after-load "projectile" '(diminish 'projectile-mode "P"))
;;   )

(use-package delight
  :ensure t
  )

(use-package emacs
  :hook (lisp-mode-hook . display-line-numbers-mode)
  :custom (display-line-numbers-type 'visual)
  )

(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

(use-package comment-dwim-2
  :ensure t
  :config (global-set-key (kbd "M-;") 'comment-dwim-2)
  )

(require 'rc-functions)
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing lines-tail newline empty
				indentation big-indent space-before-tab))
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

(electric-pair-mode -1)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs nil)
(show-paren-mode 1)

(use-package smartparens
  :disabled
  :ensure t
  ;;  :diminish smartparens-mode
  :delight ""
  :init
  (electric-pair-mode -1)
  (require 'smartparens-config)
  ;; Turn on smartparens in the minibuffer
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "M-r") 'sp-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

  ;; Handle backspace in c-like modes better for smartparens
  (bind-key [remap c-electric-backspace]
            'sp-backward-delete-char smartparens-strict-mode-map)

  ;; ;; Bind ";" to sp-comment in elisp
  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
             (eq 32 (char-after (1+ mb))))
        (and (= (1+ (line-beginning-position)) me)
             (eq 32 (char-after me)))))

  ;; Org-mode
  (sp-with-modes
      'org-mode
    (sp-local-pair "*" "*"
                   :actions '(insert wrap)
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

    ;;; Java
  (sp-with-modes
      '(java-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (smartparens-global-strict-mode 1))

(global-set-key (kbd "C-c n") 'next-multiframe-window)
(global-set-key (kbd "C-c f") 'switch-to-next-buffer)
(global-set-key (kbd "C-c p") 'switch-to-prev-buffer)

(use-package ace-window
  :ensure t
  :bind (:map global-map ("M-o" . ace-window)
	      )
  )


;(global-set-key (kbd "M-o") 'ace-window)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]))

(use-package flycheck
  :disabled t
  :ensure t
  :defer 5
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :init
  (require 'flycheck)
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  ;;  :diminish flycheck-mode
  :delight ""
  :config
  (progn
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc json-jsonlint json-python-json))
    (use-package flycheck-pos-tip
      :ensure t
      :init
      (flycheck-pos-tip-mode)
      (setq flycheck-pos-tip-timeout 10
            flycheck-display-errors-delay 0.5))
    (use-package helm-flycheck
      :ensure t
      :disabled t
      :init (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
    (use-package flycheck-haskell
      :ensure t
      :disabled t
      :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))

;;--------------------------------------------------------------------------------
;; pokazuj krańcowe nawiasy
;;--------------------------------------------------------------------------------
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq transient-mark-mode nil)


(global-set-key (kbd "M-g") 'goto-line)

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
  (setq fci-rule-width 1)
  (setq fci-rule-color "darkblue")
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.42)
  (setq fci-always-use-textual-rule t)
  (eval-after-load 'prog-mode
    (add-hook 'prog-mode-hook 'fci-mode)))
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))
;;(load "~/.emacs.d/rc/rc-adoc-mode.el")
(use-package markdown-mode :ensure t)
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
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  )

(use-package auto-complete-c-headers
  :after  auto-complete
  :ensure t
  :config
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  )

(use-package ac-c-headers
  :ensure t
  :hook
  ((c++-mode-hook c-mode-hook) . (lambda ()
		   (add-to-list 'ac-sources 'ac-source-c-headers)
		   (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
  ;; (c++-mode-hook . my:ac-c-header-init)
  )

;;(load "~/.emacs.d/rc/rc-flymake.el")
;; (load "~/.emacs.d/rc/rc-google-c-style.el")
(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode)
  )
;; (require 'rc-duplicate-thing)
(require 'rc-cc-mode)
(use-package clang-format
  :ensure t)
(use-package clang-format+
  :ensure t)

;; (require 'rc-diff-mode)
(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t))
(use-package clean-aindent-mode
  :ensure t
  :config
  (electric-indent-mode -1)
  (clean-aindent-mode)
  (eval-after-load 'prog-mode
    (lambda ()
      (progn
        (setq clean-aindent-is-simple-indent t)
        (clean-aindent-mode t))))
  :bind (:map global-map ("<RET>" . newline-and-indent))
  )

;; (require 'rc-dtrt-indent)
;; (require 'rc-ws-butler)
(use-package ws-butler
  :ensure t
  )

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
;;(setq helm-gtags-prefix-key "\C-cg")

(use-package ggtags
  :ensure t
  :defer t
  :hook ((c++-mode-hook c-mode-hook asm-mode)   . (lambda () (ggtags-mode 1)))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)
              )
  :commands ggtags-mode
  :config
  (unbind-key "M-<" ggtags-mode-map)
  (unbind-key "M->" ggtags-mode-map)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1)
                )))
  )

(setq vc-handled-backends '(git svn))
;; (use-package subword-mode
;; :diminish subword-mode)

(use-package cycle-quotes
  :ensure t
  :bind ("C-c q" . cycle-quotes))

(defun bk/last-compilation-buffer ()
  "Display last compilation buffer in current window."
  (interactive)
  (if (buffer-live-p compilation-last-buffer)
      (set-window-buffer (get-buffer-window) compilation-last-buffer)
    (message "Last compilation buffer is killed.")))

(global-set-key (kbd "C-x c") #'bk/last-compilation-buffer)

(use-package bool-flip
  :ensure t
  :bind ("C-c C-b" . bool-flip-do-flip))

(use-package company
  :ensure t
  :hook (after-init-hook . global-company-mode)
  )

(use-package dtrt-indent
  :ensure t
  :diminish t
  :config
  (setq dtrt-indent-active-mode-line-info ""))

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-defined
  :ensure t
  :init
  (add-hook 'emacs-lisp-hook #'highlight-defined-mode))

(use-package highlight-operators
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'highlight-operators-mode))

(use-package highlight-escape-sequences
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'hes-mode))

(use-package paren-face
  :ensure t
  :init (global-paren-face-mode))

(setq-default line-spacing 0)

(use-package company-c-headers
  :after company
  :ensure t
  :config
  (push 'company-c-headers company-backends))

(when (executable-find "clang")
  (use-package company-clang
    :disabled
    :ensure t)
  )

(use-package company-irony-c-headers
  :if (package-installed-p 'company-irony)
  :config
  (eval-after-load 'company
      (push 'company-irony-c-headers company-backends)))

(use-package company-irony
  :if (package-installed-p 'company-irony)
  :config
  (eval-after-load 'company
    (push 'company-irony company-backends)))

(use-package gradle-mode
  :if (package-installed-p 'gradle-mode)
  :config
  (setq gradle-executable-path "/opt/gradle/gradle-5.2.1/bin/gradle")
  (setq gradle-use-gradlew t)
  (eval-after-load 'java-mode
    (add-hook 'java-mode-hook (lambda () (gradle-mode 1)))))

(eval-after-load 'java-mode
  (add-hook 'java-mode-hook
	  (lambda ()
	    (setq fill-column 80)
	    (fci-mode)
	    (auto-fill-mode)
	    (setq c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode nil)
	    )))

;;(require 'rc-helm)
;;(require 'rc-helm-gtags)

(use-package all-the-icons :ensure t)
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


(use-package hydra :ensure t)
(use-package counsel :ensure t)
(use-package counsel-gtags
  :ensure t
  :bind (:map counsel-gtags-mode-map
	      (
	       ("M-t" . 'counsel-gtags-find-definition)
	       ("M-r" . 'counsel-gtags-find-reference)
	       ("M-s" . 'counsel-gtags-find-symbol)
	       ("M-," . 'counsel-gtags-go-backward)
	       )
	      )
  :hook ((c-mode-common-hook) . counsel-gtags-mode)
  :diminish)
(use-package counsel-projectile :ensure t)
(use-package swiper :ensure t)
(use-package ivy
  :ensure t
  :after (swiper counsel counsel-gtags)
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
         ("C-c f" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
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


(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . #'ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
	'(("default"
           ("Emacs Configuration" (or (filename . ".emacs.d")
                                      (filename . "init.el")
                                      (filename . "package.el")
                                      (filename . "private.el")
                                      (filename . "emacs.d")))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("Magit" (name . "magit"))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))
           ("Dired" (mode . dired-mode))
           ;; Dev has groups for all languages you program in
           ("Dev" (or (mode . cc-mode)
                      (filename . ".c")
                      (filename . ".cpp")
                      (filename . ".hpp")
                      (filename . ".h")
                      (filename . ".java")
                      (filename . ".properties")
                      (filename . ".gradle")
                      (filename . ".am")
                      (mode . yaml-mode))
            )
           ("Text" (or (filename . ".csv")
                       (filename . ".tsv")
                       (filename . ".txt")
                       (filename . ".log")
                       (filename . ".json")))

           ("Emacs" (or (name . "^\\*scratch\\*$")
			(name . "^\\*Messages\\*$")))
           ("Gnus" (or (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\.newsrc-dribble")))
 	   )))
  )

;; (load "~/.emacs.d/rc/rc-ecb.el")
;; (require 'rc-cedet)
;; (require 'rc-company)
;; (require 'rc-gdb)

;; :config
;; (eval-after-load "projectile" '(diminish 'projectile-mode "P"))
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
;; (use-package treemacs :ensure t)
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
(use-package magit :ensure t)
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
(use-package treemacs-magit
  :if (package-installed-p 'magit-popup)
  :ensure t)

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

(use-package rust-mode
  :ensure t
  )
(use-package cargo
  :ensure t
  )
(use-package rustic
  :ensure t
  )
(message "Init finished")
