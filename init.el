;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix -*-
;;; Commentary:
;; Load all configuration parts

;;; Code:
;; don't let Customize mess with my .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "karol.barski@tieto.com")

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 4 1024 1024))

(setq mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(load "~/.emacs.d/rc/environment.el")

;; blink screen on bell
(setq visible-bell t)
;;(setq debug-on-error nil)

;; dim the ignored part of the file name
;;(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

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
                      (setq scroll-bar-width 10)
                      )
                  )))
  (if (display-graphic-p)
      (progn
        (tool-bar-mode 0)               ; turn menus off
        (menu-bar-mode 0)               ; disable toolbar
        (set-scroll-bar-mode 'right) ; scroll bar on the right side
        (setq scroll-bar-width 10)
        (scroll-bar-mode 'right)
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

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format "Emacs: %b %+%+ %f ")

(if (version< emacs-version "24")
    ;; install elpa on emacs 23
    (progn
      ;; (let ((buffer (url-retrieve-synchronously
      ;;             "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el")))
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
;; (setq package-check-signature nil)
;; (require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this  warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;;(add-to-list 'package-archives
;;           '("gnu" . "http://elpa.gnu.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (version< emacs-version "24")
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed.
Return a list of installed packages or nil ofr every skipped package.

PACKAGES: list of packages to install."
  (mapcar
   (lambda (package)
     ;;(package-installed-p 'evil)
     (if (package-installed-p package)
         package
       (package-install package)))
   packages))

;; Bootstrap `use-package`
(condition-case nil
    (require 'use-package)
  (file-error
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(setq-default use-package-compute-statistics t)
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-check-before-init t)

(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (line-spacing 0)
  ;; (help-char "? M-?")
  :config
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (define-key help-map "?" 'describe-key-briefly)
  :bind
  ("C-c f" . 'switch-to-next-buffer)
  ("C-c p" . 'switch-to-prev-buffer)
  ;; ("C-h" . 'delete-backward-char)
  ;; ("C-?" . 'delete-char)
  ;; ("ESC C-h" . 'backward-kill-word)
  ;; ("ESC C-?" . 'kill-word)
  ;; ("<f1>" . 'help-command)
  ;; ("ESC ?" . 'help-command)
  ;; ("ESC ? F" . 'view-emacs-FAQ)
  )

;; ignore case when reading a file name completion
(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t)
  )

(use-package simple
  :custom
  (global-mark-ring-max 5000)         ; increase mark ring to contains 5000 entries
  (mark-ring-max 5000)                ; increase kill ring to contains 5000 entries
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (kill-ring-max 5000) ; increase kill-ring capacity
  (kill-whole-line t)
  (transient-mark-mode nil)
  ;; remap C-H to backspace
  (normal-erase-is-backspace t)
  :hook
  ;; remap C-H to backspace
  (terminal-init-xterm . (lambda () (normal-erase-is-backspace-mode t)))
  :config
  (put 'set-goal-column 'disabled nil)
  :bind
  ("C-;" . kill-whole-line))

(use-package vc
  :custom
  (vc-follow-symlinks t)
  )

(use-package frame
  ;;--------------------------------------------------------------------------------
  ;; turn off blinking cursor
  ;;--------------------------------------------------------------------------------
  :custom
  (blink-cursor-blinks 3)
  (blink-cursor-delay 1)
  :config
  (blink-cursor-mode)
  :bind
  ("C-c n" . next-multiframe-window))

(use-package help
  :config
  (temp-buffer-resize-mode t))
(use-package image
  :config
  (auto-image-file-mode 1))
(use-package time
  :custom
  (display-time-format "%H:%M %d/%m/%Y")
  (display-time-24hr-format t)
  :config
  (display-time)
  )

(use-package autoinsert
  :disabled
  :config
  (auto-insert-mode 1)

  ;; autoinsert C/C++ header
  (define-auto-insert
    (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
    '(nil
      (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
             (nopath (file-name-nondirectory noext))
             (ident (concat (upcase nopath) "_H")))
        (concat "#ifndef " ident "\n"
                "#define " ident  " 1\n\n\n"
                "\n\n#endif // " ident "\n"))
      ))
  ;; auto insert C/C++
  (define-auto-insert
    (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
    '(nil
      (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
             (nopath (file-name-nondirectory noext))
             (ident (concat nopath ".h")))
        (if (file-exists-p ident)
            (concat "#include \"" ident "\"\n")))
      ))
  )

(use-package delight :ensure t)

;;--------------------------------------------------------------------------------
;; kolorowanie składni
;;--------------------------------------------------------------------------------
(use-package font-lock
  :custom
  (font-lock-maximum-decoration t)
  :config
  (global-font-lock-mode t)
  )

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2)
  )
(if (fboundp 'banner-comment)
    (use-package banner-comment
      :bind ("M-'" . banner-comment)
      )
  )

(use-package smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-c v")))

(add-to-list 'load-path "~/.emacs.d/rc")
(load "~/.emacs.d/rc/rc-functions.el")

(use-package whitespace
  :custom
  (whitespace-style '(face trailing lines-tail newline empty
                           indentation big-indent space-before-tab))
  (whitespace-line-column '100)
  :bind
  ("C-c w" . whitespace-mode)
  )

;; individual modes loading
;; (use-package cua-base
;;   :custom
;;   (cua-enable-cua-keys nil)
;;   (cua-highlight-region-shift-only t) ;; no transient mark mode
;;   (cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no  transient-mark-mode
;;   :config
;;   (cua-mode)
;;   :bind
;;   ("C-<RET>" . cua-rectangle-mark-mode))

(use-package yasnippet
  :if (package-installed-p 'yasnippet)
  :defines (yas-snippet-dirs yas-active-snippets)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/rc/snippets")
  (yas-global-mode 1)
  ;; Inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))
  :custom
  (yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  :hook (term-mode . (lambda() (setq yas-dont-activate-functions t)))
  :bind (:map yas-keymap
              ("<return>" . yas-exit-all-snippets)
              ("C-e" . yas/goto-end-of-active-field)
              ("C-a" . yas/goto-start-of-active-field)))
(use-package yasnippet-snippets
  :if (package-installed-p 'yasnippet)
  :ensure)
(use-package yasnippet-classic-snippets
  :if (package-installed-p 'yasnippet)
  :ensure)

(use-package ivy-yasnippet
  :if (package-installed-p 'yasnippet)
  :ensure ivy-yasnippet)

(use-package anzu
  :ensure t
  :custom
  (anzu-mode-ligther "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  :bind
  (:map isearch-mode-map (([remap isearch-query-replace] . anzu-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-query-replace-regexp))))

(if (not (package-installed-p 'smartparens))
    (use-package paren
      :custom ((show-paren-style 'parenthesis)
               (show-paren-when-point-inside-paren t)
               (show-paren-when-point-in-periphery t))
      :config (show-paren-mode 1))

  (use-package smartparens
    :delight ""
    ;; :bind
    ;; (:map smartparens-mode-map
    ;;  (("C-M-f" . sp-forward-sexp)
    ;;   ("C-M-b" . sp-backward-sexp)
    ;;   ("C-M-<right>" . sp-forward-sexp)
    ;;   ("C-M-<left>" . sp-backward-sexp)

    ;;   ("M-F" . sp-forward-sexp)
    ;;   ("M-B" . sp-backward-sexp)

    ;;   ("C-M-d" . sp-down-sexp)
    ;;   ("C-M-a" . sp-backward-down-sexp)
    ;;   ("C-S-d" . sp-beginning-of-sexp)
    ;;   ("C-S-a" . sp-end-of-sexp)

    ;;   ("C-M-e" . sp-up-sexp)
    ;;   ("C-M-u" . sp-backward-up-sexp)
    ;;   ("C-M-t" . sp-transpose-sexp)

    ;;   ("C-M-n" . sp-next-sexp)
    ;;   ("C-M-p" . sp-previous-sexp)

    ;;   ("C-M-k" . sp-kill-sexp)
    ;;   ("C-M-w" . sp-copy-sexp)

    ;;   ("M-r" . sp-unwrap-sexp)

    ;;   ("C-(" . sp-forward-barf-sexp)
    ;;   ("C-)" . sp-forward-slurp-sexp)
    ;;   ("M-(" . sp-forward-barf-sexp)
    ;;   ("M-)" . sp-forward-slurp-sexp)

    ;;   ("M-D" . sp-splice-sexp))
    ;;  :map emacs-lisp-mode-map (";" . sp-comment)
    ;;  :map smartparens-strict-mode-map ([remap c-electric-backspace] . sp-backward-delete-char))
    ;; :hook
    ;; ((minibuffer-setup . turn-on-smartparens-strict-mode)
    ;;  (c-mode-common . (lambda () (require 'smartparens-c)))
    ;;  (org-mode . (lambda () (require 'smartparens-org))))
    :custom
    (sp-base-key-bindings 'paredit)
    (sp-autoskip-closing-pair 'always)
    (sp-hybrid-kill-entire-symbol 'nil)

    :config
    (electric-pair-mode -1)
    (require 'smartparens-config)
    ;; (smartparens-global-strict-mode 1)
    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1)
    )
  )

;;--------------------------------------------------------------------------------
;; recentf mode
;;--------------------------------------------------------------------------------
(use-package recentf
  :config
  (recentf-mode t)
  :bind
  ("<f7>" . recentf-open-files))

;; keep minibuffer history between session
(use-package savehist
  :config
  (savehist-mode t))

(use-package ace-window
  :disabled
  :ensure t
  :bind (:map global-map ("M-o" . ace-window))
  )
(use-package winum
  :ensure t
  :custom
  (winum-auto-setup-mode-line t)
  :config
  (winum-mode)
  )

(use-package editorconfig
  :ensure t
  :delight ""
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :defer 5
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]
      ))
  (when (package-installed-p 'hydra)
    (defhydra hydra-flycheck
      (global-map "C-c ! j"
                  :pre (flycheck-list-errors)
                  :post (quit-windows-on "*Flycheck errors*")
                  :hint nil)
      "Errors"
      ("f" flycheck-error-list-set-filter "Filter")
      ("j" flycheck-next-error "Next")
      ("k" flycheck-previous-error "Previous")
      ("gg" flycheck-first-error "First")
      ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
      ("q" nil))
    )
  )

(use-package flycheck-checkpatch
  :disabled
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-checkpatch-setup)
  ;; :config
  ;; (defadvice flycheck-checkpatch-set-executable (after kb/flycheck-checkpatch-executable activate)
  ;;   "Setup flycheck-checkpatch-executable if function couldn't pickup it from project."
  ;;   (let ((my-checkpatch (executable-find "~/.emacs.d/scripts/checkpatch.pl")))
  ;;     (message "running advice for flycheck-checkpatch-set-executable %S -> %S" flycheck-checkpatch-executable my-checkpatch)
  ;;     (unless (and flycheck-checkpatch-executable (executable-find flycheck-checkpatch-executable))
  ;;    (setq-local flycheck-checkpatch-executable my-checkpatch))))
  )
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-analyzer-setup))
(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-tidy-setup))
(use-package avy-flycheck
  :ensure t
  :after flycheck
  :hook (flycheck-mode . avy-flycheck-setup))
(use-package ivy-avy :ensure t)

;;--------------------------------------------------------------------------------
;; pokazuj krańcowe nawiasy
;;--------------------------------------------------------------------------------
(use-package custom
  :bind
  ("C-c o" . kb/switch-font)
  :config
  (defvar kb/terminal-theme 'wombat)
  (defvar kb/window-theme 'misterioso)
  (defvar kb/theme-window-loaded nil)
  (defvar kb/theme-window-font (if (eq system-type 'windows-nt)
                                   "Unifont"
                                        ;(set-frame-parameter nil 'font "Arial Unicode MS")
                                 "DejaVu Sans Mono"))
  (defvar kb/theme-terminal-loaded nil)
  (defvar kb/theme-original-font nil)

  ;; font configuration
  (defun kb/set-window-font ()
    "Function set screen font.
If Emacs is run in MS Windows then use Arial Unicode MS
On U*x systems Use DejaVu Sans Mono"
    (setq kb/theme-original-font (frame-parameter nil 'font))
    (set-frame-parameter nil 'font kb/theme-window-font))

  (defun kb/switch-font ()
    "Function set screen font.
Set original font."
    (interactive)
    (if (and kb/theme-original-font (eq kb/theme-original-font (frame-parameter nil 'font)))
        (kb/set-window-font)
      (set-frame-parameter nil 'font kb/theme-original-font)))

  (defun kb/load-grapics-theme ()
    (interactive)
    (unless kb/theme-window-loaded
      (setq kb/theme-window-loaded (load-theme kb/window-theme t t))
      (message "Theme `%S' loaded %S" kb/window-theme kb/theme-window-loaded))
    kb/theme-window-loaded)

  (defun kb/load-terminal-theme ()
    (interactive)
    (unless kb/theme-terminal-loaded
      (setq kb/theme-terminal-loaded (load-theme kb/terminal-theme t t))
      (message "Theme `%S' loaded %S" kb/terminal-theme kb/theme-terminal-loaded))
    kb/theme-terminal-loaded)

  (defun kb/activate-frame-theme (frame)
    "Activate theme depending on current FRAME window system.

If theme is'n loaded then it will be loaded at first"
    (interactive)
    (select-frame frame)
    (if (window-system frame)
        (progn
          (if (kb/load-grapics-theme)
              (enable-theme kb/window-theme))
          (kb/set-window-font))
      (if (kb/load-terminal-theme)
          (enable-theme kb/terminal-theme))))

  (defun kb/activate-theme (&optional frame)
    "Set theme on active FRAME."
    (interactive)
    (let ((frame (or frame (selected-frame))))
      (if (frame-focus-state frame)
          (kb/activate-frame-theme frame))))
  (kb/activate-theme)

  (add-function :after after-focus-change-function #'kb/activate-theme)
  :hook (after-make-frame-functions . kb/load-frame-theme))


(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup)
  )

(use-package fill-column-indicator
  :disabled
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
  :delight ""
  :config (global-undo-tree-mode))

(use-package markdown-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )
(use-package highlight-doxygen
  :ensure t
  :hook
  (c-mode-common . highlight-doxygen-mode)
  )

(use-package cmake-mode
  :if (package-installed-p 'cmake-mode)
  :defines (cmake-tab-width)
  :custom
  (auto-mode-alist
   (append '(("CMakeLists\\.txt\\'" . cmake-mode)) auto-mode-alist))
  :hook
  (cmake-mode . (lambda ()
                  (message "CmakeMode custom")
                  (setq fill-column 80)
                  (auto-fill-mode)
                  (setq cmake-tab-width 4)
                  (setq indent-tabs-mode nil))))


(use-package pabbrev
  :delight
  :ensure t)

(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

;; (defun kb/cc-compile-command-hook ()
;;     "Compile C/C++ files with gcc if makefile doesn't exist."
;;   (unless (or (file-exists-p "makefile")
;;               (file-exists-p "Makefile"))
;;     (set (make-local-variable 'compile-command)
;;          (let ((file (file-name-nondirectory buffer-file-name)))
;;            (format "%s -c -o %s.o %s %s %s"
;;                    (or (getenv "CC") "g++")
;;                    (file-name-sans-extension file)
;;                    (or (getenv "CPPFLAGS") "-DDEBUG=9")
;;                    (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
;;                    file)))))

;; (Use-package cc-mode
;;   :config
;;   (add-hook 'c-mode  (lambda ()
;;                     (setq fill-column 100)
;;                     (auto-fill-mode)
;;                     (fci-mode)
;;                     (setq tab-width 8)
;;                     (setq indent-tabs-mode t)
;;                     ))
;;   (add-hook 'c++-mode (lambda ()
;;                      (setq fill-column 100)
;;                      (auto-fill-mode)
;;                      (fci-mode)
;;                      (setq tab-width 4)
;;                      (setq indent-tabs-mode nil)
;;                      ))
;;   (add-hook 'c++-mode 'kb/cc-compile-command-hook)
;; )

(use-package cc-mode
  :config
  (font-lock-add-keywords 'c-mode '("\\<\\(and\\|or\\|not\\)\\>"))
  )

(use-package prog-mode
  :config
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\(\\(TODO\\|XXX\\)\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
                            ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
                            ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'visual)
  :hook
  ((emacs-lisp-mode lisp-mode c-mode-common) . display-line-numbers-mode)
  )

(use-package diff-mode
  :hook
  (diff-mode . (lambda ()
                 (setq-local whitespace-style
                             '(face
                               tabs
                               tab-mark
                               spaces
                               space-mark
                               trailing
                               indentation::space
                               indentation::tab
                               newline
                               newline-mark))
                 (whitespace-mode 1)
                 (if (file-exists-p "./scripts/checkpatch.pl")
                     (progn (print "setting compile-command")
                            (set (make-local-variable 'compile-command)
                                 (concat "./scripts/checkpatch.pl --emacs "
                                         (buffer-file-name))))
                   (print "checkpatch not found"))))
  )
(use-package volatile-highlights
  :ensure t
  :after undo-tree
  :custom
  (Vhl/highlight-zero-width-ranges t)
  :config (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )
(use-package clean-aindent-mode
  :disabled t
  :ensure t
  :custom (clean-aindent-is-simple-indent t)
  :config (electric-indent-mode -1)
  :bind (:map global-map ("<RET>" . newline-and-indent)))

(use-package ws-butler
  :ensure t
  :hook
  ((c-mode-common text-mode fundamental-mode) . ws-butler-mode))

(use-package ggtags
  :if (package-installed-p 'ggtags)
  :defines (ggtags-mode-map)
  :hook ((c-mode-common . (lambda ()
                            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                              (ggtags-mode 1)))))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)
              )
  :config
  (unbind-key "M-<" ggtags-mode-map)
  (unbind-key "M->" ggtags-mode-map))

(use-package vc-hooks
  :custom
  (vc-handled-backends '(git svn)))
(use-package subword-mode
  :delight
  :hook
  (c-mode-common . subword-mode))

(use-package cycle-quotes
  :ensure t
  :bind ("C-c q" . cycle-quotes))

(use-package compile
  :config
  (defun bk/last-compilation-buffer ()
    "Display last compilation buffer in current window."
    (interactive)
    (if (buffer-live-p compilation-last-buffer)
        (set-window-buffer (get-buffer-window) compilation-last-buffer)
      (message "Last compilation buffer is killed.")))
  :bind (:map global-map ("C-x c" . bk/last-compilation-buffer))
  )

(use-package bool-flip
  :ensure t
  :bind ("C-c C-b" . bool-flip-do-flip))

(use-package rc-functions
  :config
  (defun set-buffer-eol-unix () (set-buffer-file-eol-type 'unix))
  (defun set-buffer-eol-dos () (set-buffer-file-eol-type 'dos))
  (defun set-buffer-eol-mac () (set-buffer-file-eol-type 'mac))
  :bind
  ("C-'" . comment-or-uncomment-region-or-line)
  ("C-j" . kb-join-line)
  ("C-a" . prelude-move-beginning-of-line)
  ("C-c i" . indent-region-or-buffer)
  ("M-o" . prelude-smart-open-line)
  ("%" . match-parenthesis) ;; % key on paren moves cursor to matching paren
  ("C-c T" . kb-delete-trailing-whitespaces-and-untabify)
  ("C-c u" . set-buffer-eol-unix)
  ("C-c d" . set-buffer-eol-dos)
  ("C-c m" . set-buffer-eol-mac)
  ("C-c C-d" . insert/date-time))

(use-package rect
  :bind
  ;; string-insert-rectangle is useful but not binded to any key by default
  ("C-x r a" . string-insert-rectangle)
  ("C-x r l" . rectangle-number-lines)
  )

(use-package amx :ensure t)
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (delete 'company-semantic company-backends)
  (delete 'company-gtags company-backends)
  ;;(add-hook 'c-mode-common (lambda ()
  ;;                         (add-to-list (make-local-variable 'company-backends) 'company-yasnippet)))
  :bind
  ;; ((:map c-mode-map ("TAB" . company-complete))
  ;;  (:map c++-mode-map ("TAB" . company-complete)))
  (:map global-map ([remap dabbrev-expand] . company-dabbrev))
  )

(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

(use-package company-box
  :ensure t
  :after company
  :custom
  (company-box-frame-behavior 'default)
  :hook (company-mode . company-box-mode)
  :config
  ;; company-capf menu colors

  (add-to-list 'company-box-backends-colors
               '(company-capf . (:candidate (:background "lime green" :foreground "black")
                                            :annotation (:background "green" :foreground "black")
                                            :selected (:background "yellow" :foreground "black")
                                            )))
  ;; company-clang menu colors
  (add-to-list 'company-box-backends-colors
               '(company-clang . (:candidate (:background "dark blue" :foreground "white")
                                             :annotation (:background "dark blue" :foreground "white")
                                             :selected (:background "gold" :foreground "dark blue"))))
  ;; company-dabbrev menu colors

  (add-to-list 'company-box-backends-colors
               '(company-dabbrev . (:candidate (:background "grey" :foreground "green")
                                               :annotation (:background "grey" :foreground "green")
                                               :selected (:background "chartreuse" :foreground "dark blue"))))

  (add-to-list 'company-box-backends-colors
               '(company-dabbrev-code . (:candidate (:background "magenta" :foreground "GreenYellow"))))
  )

(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (if (boundp 'company-box-backends-colors)
      (add-to-list 'company-box-backends-colors
                   '(company-c-headers . (:candidate (:background "wheat" :foreground "black")
                                          :annotation (:background "grey" :foreground "green")
                                          :selected (:background "PaleVioletRed1" :foreground "SaddleBrown")
                                                         ))))
  )

(use-package company-ctags
  :ensure t
  :after company
  :config
  (company-ctags-auto-setup))

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode)
  )

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode #'highlight-defined-mode)
  )

(use-package highlight-operators
  :ensure t
  :hook (c-common-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :ensure t
  :pin "melpa"
  :config (hes-mode))

;;(use-package paren-face
;;   :ensure t
;;   :init (global-paren-face-mode))

(when (executable-find "clang-format")
  (use-package clang-format
    :ensure t
    :bind
    (:map global-map ("C-M-'" . clang-format-region)))
  (use-package clang-format+
    :ensure t
    :custom (clang-format+-context 'modification)
    ;;:hook (c-mode-common . clang-format+-mode)
    )
  )

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ;; (lsp-print-performance t)
  ;; (lsp-enable-xref t)
  (lsp-idle-delay 1.0)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable t)

  :hook
  ((c++-mode . lsp-deferred)
   (c-mode . lsp-deferred))
  :bind (:map lsp-mode-map ("M-." . lsp-find-declaration))
  :config
  (if (package-installed-p 'yasnippet)
      (setq lsp-enable-snippet t)
    (setq lsp-enable-snippet nil)
    )
  (add-hook 'lsp-headerline-breadcrumb-mode-hook
            (lambda ()
              "Fix headerlime colors for breadcrumbs"
              (set-face-foreground 'lsp-headerline-breadcrumb-symbols-face "yellow")
              (set-face-background 'lsp-headerline-breadcrumb-symbols-face "forest green")
              (set-face-foreground 'lsp-headerline-breadcrumb-prefix-face "yellow")
              (set-face-background 'lsp-headerline-breadcrumb-prefix-face "dark green")
              (set-face-foreground 'lsp-headerline-breadcrumb-separator-face "yellow")
              (set-face-background 'lsp-headerline-breadcrumb-separator-face "lime green")
              (set-face-foreground 'lsp-headerline-breadcrumb-project-prefix-face "yellow")
              (set-face-background 'lsp-headerline-breadcrumb-project-prefix-face "lime green")
              (set-face-foreground 'header-line "yellow")
              (set-face-background 'header-line "dark green"))
  ))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-log-io nil)
  (setq flycheck-checker-error-threshold 10000)
  )
(use-package lsp-ivy
  :ensure t
  :commands
  (lsp-ivy-workspace-symbol)
  )
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
(use-package dap-mode :ensure t)
(use-package dap-gdb-lldb :after dap-mode)
(use-package which-key :ensure :config (which-key-mode))

(use-package spaceline
  :ensure t
  :config
  (defvar kb/line-selected-window (frame-selected-window))
  (defun kb/line-selected-window-active-p ()
    (eq kb/line-selected-window (selected-window)))
  (defface kb/line-modified-face
    `((t (:foreground "#8be9fd" :background nil)))
    "Modeline modified-file face"
    :group `kb)
  (defface kb/line-modified-face-inactive
    `((t (:foreground "#6272a4" :background nil)))
    "Modeline modified-file face for inactive windows"
    :group `kb)
  (defface kb/line-read-only-face
    `((t (:foreground "#ff5555")))
    "Modeline readonly face."
    :group `kb)
  (defface kb/line-read-only-face-inactive
    `((t (:foreground "#aa4949")))
    "Modeline readonly face for inactive windows."
    :group `kb)
  (defface kb/line-buffer-name-face
    `((t (:inherit 'font-lock-type-face)))
    "Modeline buffer name face."
    :group `kb)
  (setq-default mode-line-format
                (list
                 " "
                 mode-line-misc-info ; for eyebrowse

                 '(:eval (when-let (vc vc-mode)
                           (list " "
                                 (propertize (substring vc 5)
                                             'face 'font-lock-comment-face)
                                 " ")))

                 '(:eval (list
                          ;; the buffer name; the file name as a tool tip
                          (propertize " %b" 'face 'font-lock-type-face
                                      'help-echo (buffer-file-name))
                          (when (buffer-modified-p)
                            (propertize
                             " "
                             'face (if (kb/line-selected-window-active-p)
                                       'kb/line-modified-face
                                     'kb/line-modified-face-inactive)))
                          (when buffer-read-only
                            (propertize
                             ""
                             'face (if (kb/line-selected-window-active-p)
                                       'kb/line-read-only-face
                                     'kb/line-read-only-face-inactive)))
                          " "))

                 ;; relative position in file
                 (propertize "%p" 'face 'font-lock-constant-face)

                 ;; spaces to align right
                 '(:eval (propertize
                          " " 'display
                          `((space :align-to (- (+ right right-fringe right-margin)
                                                ,(+ 3 (string-width mode-name)))))))

                 ;; the current major mode
                 (propertize " %m " 'face 'font-lock-string-face)))
  )

;;; (use-package spaceline
;;;   :ensure t
;;;   :config
;;;   (defface kb/line-modified-face
;;;     `((t (:foreground "#8be9fd" :background nil)))
;;;     "Modeline modified-file face"
;;;     :group `kb)
;;;   (defface kb/line-modified-face-inactive
;;;     `((t (:foreground "#6272a4" :background nil)))
;;;     "Modeline modified-file face for inactive windows"
;;;     :group `kb)
;;;   (defface kb/line-read-only-face
;;;     `((t (:foreground "#ff5555")))
;;;     "Modeline readonly face."
;;;     :group `kb)
;;;   (defface kb/line-read-only-face-inactive
;;;     `((t (:foreground "#aa4949")))
;;;     "Modeline readonly face for inactive windows."
;;;     :group `kb)
;;;   (defface kb/line-buffer-name-face
;;;     `((t (:inherit 'font-lock-type-face)))
;;;     "Modeline buffer name face."
;;;     :group `kb)
;;;   (defvar kb/line-selected-window (frame-selected-window))
;;;   (defun kb/line-set-selected-window (&rest _args)
;;;     (when (not (minibuffer-window-active-p (frame-selected-window)))
;;;       (setq kb/line-selected-window (frame-selected-window))
;;;       (force-mode-line-update)))
;;;   (defun kb/line-unset-selected-window ()
;;;     (setq kb/line-selected-window nil)
;;;     (force-mode-line-update))
;;;
;;;   (defun kb/line-selected-window-active-p ()
;;;     (eq kb/line-selected-window (selected-window)))
;;;
;;;   (defun kb/line-scan-active-frame ()
;;;     "Scan frames and set modeline depending on frame focus."
;;;     (interactive)
;;;     (if (frame-focus-state)
;;;     (kb/line-set-selected-window)
;;;       (kb/line-unset-selected-window)))
;;;
;;;   (setq-default mode-line-format
;;;               (list
;;;                " "
;;;                mode-line-misc-info ; for eyebrowse
;;;
;;;                '(:eval (when-let (vc vc-mode)
;;;                          (list " "
;;;                                (propertize (substring vc 5)
;;;                                            'face 'font-lock-comment-face)
;;;                                " ")))
;;;
;;;                '(:eval (list
;;;                         ;; the buffer name; the file name as a tool tip
;;;                         (propertize " %b" 'face 'font-lock-type-face
;;;                                     'help-echo (buffer-file-name))
;;;                         (when (buffer-modified-p)
;;;                           (propertize
;;;                            " "
;;;                            'face (if (kb/line-selected-window-active-p)
;;;                                      'kb/line-modified-face
;;;                                    'kb/line-modified-face-inactive)))
;;;                         (when buffer-read-only
;;;                           (propertize
;;;                            ""
;;;                            'face (if (kb/line-selected-window-active-p)
;;;                                      'kb/line-read-only-face
;;;                                    'kb/line-read-only-face-inactive)))
;;;                         " "))
;;;
;;;                ;; relative position in file
;;;                (propertize "%p" 'face 'font-lock-constant-face)
;;;
;;;                ;; spaces to align right
;;;                '(:eval (propertize
;;;                         " " 'display
;;;                         `((space :align-to (- (+ right right-fringe right-margin)
;;;                                               ,(+ 3 (string-width mode-name)))))))
;;;
;;;                ;; the current major mode
;;;                (propertize " %m " 'face 'font-lock-string-face)))
;;;
;;;   (add-function :after after-focus-change-function #'kb/line-scan-active-frame)
;;;   :hook
;;;   (window-configuration-change . kb/line-set-selected-window)
;;;   )


(when (package-installed-p 'all-the-icons)
  (use-package all-the-icons)
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))
  ;; (use-package all-the-icons-ibuffer :ensure t
  ;;   :hook (ibuffer-load-hook . (lambda () (all-the-icons-ibuffer-mode 1))))
  (use-package treemacs-all-the-icons :ensure t)
  (use-package treemacs-icons-dired :ensure t)
  (use-package all-the-icons-ivy
    :ensure t
    :config
    (all-the-icons-ivy-setup))
  (use-package all-the-icons-ivy-rich
    :ensure t
    :config (all-the-icons-ivy-rich-mode 1))
  (use-package spaceline-all-the-icons
    :after (spaceline all-the-icons)
    :disabled
    :config
    (spaceline-all-the-icons-theme))
  )

(use-package neotree
  :ensure t
  :bind
  ([f8] . neotree-toggle)
  :custom
  ;; Every time when the neotree window is opened, let if find current
  ;; file and jump to node.
  (neo-smart-open t)
  ;; track 'projectile-switch-project' (C-c p p),
  (projectile-switch-project-action 'neotree-projectile-action)
  :config
  ;; needs package all-the-icons
  ;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook
            (lambda (&rest _) (display-line-numbers-mode -1)))
  )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-goto (global-map "M-g")
    "goto"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

;; (use-package hercules :ensure t)

(use-package counsel
  :ensure t
  :custom
  (counsel-file-file-at-point t)
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  :config
  (counsel-mode 1)
  :bind (
         ("<f2> j". counsel-set-variable)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f7>" . counsel-recentf)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c L" . counsel-git-log)
         ("C-c J" . counsel-file-jump)
         ("C-x l" . counsel-locate)
         ("C-c t" . counsel-load-theme)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  )
;;(use-package multiple-cursors :ensure t)

(use-package counsel-projectile
  :ensure t
  :after counsel projectile)
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch)
  )


(use-package ivy
  :demand
  :config
  (ivy-mode 1)
  :bind (
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         )
  :custom
  (ivy-use-virutal-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  )

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

(use-package gdb-mi
  :custom
  (gdb-many-windows t)   ;; use gdb-many-windows by default
  (gdb-show-main t) ;; Non-nil means display source file containing the main routine at startup
  )

;; load generic modes which support e.g. batch files
;;(use-package generic-x)

(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))


(use-package projectile
  :if (package-installed-p 'projectile)
  :delight '(:eval (concat " " (projectile-project-name)))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  :config (projectile-mode)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package speedbar
  :custom
  ;; number of spaces used for indentation
  (speedbar-indentation-width 2)
  :config
  ;; expand/collapse latex sections
  (speedbar-add-supported-extension '(".tex" ".bib" ".w"))
  :bind (("<f4>" . speedbar-get-focus)   ;; jump to speedbar frame
         :map speedbar-mode-map
         ;; bind the arrow keys in the speedbar tree
         ("<right>" . speedbar-expand-line)
         ("<left>" . speedbar-contract-line)))

(use-package graphviz-dot-mode
  :if (package-installed-p 'graphviz-dot-mode)
  :custom (graphviz-dot-view-command "xdot %s"))

(use-package groovy-mode
  :if (package-installed-p 'groovy-mode))

;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :custom
;;   (py-shell-name "ipython")
;;   (py-which-bufname "IPython")
;;   (py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;;   (py-force-py-shell-name-p t)
;;   (py-shell-switch-buffers-on-execute-p t)
;;   (py-switch-buffers-on-execute-p t)
;;   (py-split-windows-on-execute-p nil)
;;   (py-smart-indentation t)

(use-package magit :ensure t)
(use-package treemacs-magit
  :if (package-installed-p 'magit-popup)
  :ensure t)

(message "Init finished")
;;; init.el ends here
