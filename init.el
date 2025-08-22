;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix; lexical-binding: t; -*-
;;; Commentary:
;; Load all configuration parts

(message "** Init entered")

;; useful for quickly debugging Emacs
(setq debug-on-error t)
(setenv "LSP_USE_PLISTS" "true")
;;; Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;;
(setq user-full-name "Karol Barski")

;; TODO:
;;  Check this init
;; https://github.com/alexmurray/dot_emacs.d/blob/master/init.el
;; https://github.com:jcs-emacs/jcs-emacs.git

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

(defgroup kb-config nil
  "Custom options for KB config."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/lollinus/my-emacs-config"))

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(if (version< emacs-version "30.1")
    (setopt idle-update-delay 1.0)  ; default is 0.5
  (setopt which-func-update-delay 1.0))

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      inhibit-startup-screen t)
(setq user-mail-address "karol.barski@cognizant.com")
(setq line-spacing 0)

(setopt sentence-end-double-space nil)


;;; Code:
;; don't let Customize mess with my .emacs
(defconst rc-directory (locate-user-emacs-file "rc/")
  "Variable storing path to RC directory.
By default is subdirectory of `user-emacs-directory'.")
(message "Adding `%S' to `load-path'" rc-directory)
(add-to-list 'load-path rc-directory)

(defconst kb/lisp-directory (locate-user-emacs-file "site-lisp")
  "Variable storing path to local lisp directory.
By default is subdirectory of `user-emacs-directory'.")
(message "Adding `%S' to `load-path'" kb/lisp-directory)
(add-to-list 'load-path kb/lisp-directory)

(when (eq system-type 'windows-nt)
  (setopt w32-pipe-read-delay '-1))

(setq custom-file (locate-user-emacs-file "custom.el"))
;; load custom but ignore error if doesn't exist
(load custom-file 'noerror 'nomessage)

(add-to-list 'load-path (expand-file-name "rc" user-emacs-directory))
(require 'rc-functions (expand-file-name "rc-functions.el" rc-directory) t)
(require 'kb-secrets (expand-file-name "kb-secrets.el" rc-directory) t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 3 1024 1024)) ; 3MB
(setq tab-width 4)                       ; default to 4 visible spaces to display a tab
(setq indicate-empty-lines t)

;; blink screen on bell
(setq visible-bell t)
;;(setq debug-on-error nil)

;; dim the ignored part of the file name
;;(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

(setq mouse-highlight 10)
(setq make-pointer-invisible t)


;;--------------------------------------------------------------------------------
;; My customized emacs
;;--------------------------------------------------------------------------------
;; fancy streching cursor
(setq x-stretch-cursor nil)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format '((:eval (if (buffer-file-name)
                                      (concat (abbreviate-file-name (buffer-file-name)) " %+%+ ")
                                    "%b %+%+ %f"))))
(setq scroll-margin 10
      scroll-conservatively 100000
      scroll-preserve-screen-position 'always)


;; =========================== leaf bootstrap ==========================
;; <leaf-install-code>
;; Make it easy to jump between packages
;; (with-eval-after-load 'package (message "**** Package loaded"))
(message "leaf bootstrap begin")
(eval-and-compile
  (customize-set-variable 'package-archives
                          '(("org" . "https://orgmode.org/elpa/")
                            ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
                            ("melpa" . "https://melpa.org/packages/")
                            ("gnu" . "https://elpa.gnu.org/packages/")
                            ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
  (customize-set-variable 'package-archive-priorities
      '(("gnu"      . 0)
        ("nongnu"   . 0)
        ("melpa"    . 5)
        ("jcs-elpa" . 10)))

;;  (setq package-enable-at-startup nil  ; To avoid initializing twice
  ;;        package-check-signature nil)

  ;;(require 'package)

  ;; (when noninteractive (package--archives-initialize))
  ;; (package--archives-initialize)
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
(message "leaf bootstrap done")
;; </leaf-install-code>
;; =========================== leaf bootstrap ==========================

(leaf leaf-convert :ensure t)

(leaf dashboard
  :doc "A startup screen extracted from Spacemacs"
  :req "emacs-27.1"
  :tag "dashboard" "tools" "screen" "startup" "emacs>=27.1"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :added "2025-02-27"
  :emacs>= 27.1
  :ensure t
  :bind (dashboard-mode-map
         ("<up>" . previous-line)
         ("<down>"  . next-line)
         ("C-k C-p" . package-list-packages)
         )
  :require t
  :init
  (when (package-installed-p 'nerd-icons)
    (setopt dashboard-icon-type 'nerd-icons))
  (when (package-installed-p 'projectile)
    (setopt dashboard-projects-backend 'projectile))

  :config
  (message "**** Configure dashboard")
  (dashboard-setup-startup-hook)
  (setopt initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq dashboard-items '((projects . 5)
                          (recents . 10)
                          (bookmarks . 5)
                          ;; (agenda . 5)
                          (registers . 5)
                          ))

  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (setq dashboard-navigation-cycle t)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  ;; (setq dashboard-show-shortcuts nil)
  )

(leaf transient
  :doc "Transient commands"
  :req "emacs-25.1" "compat-29.1.3.4"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/magit/transient"
  :added "2023-02-19"
  :emacs>= 25.1
  :ensure t
  :after compat
  :config
  (leaf transient-dwim
    :doc "Useful preset transient commands"
    :req "emacs-26.1" "transient-0.1"
    :tag "tools" "emacs>=26.1"
    :url "https://github.com/conao3/transient-dwim.el"
    :added "2025-01-21"
    :emacs>= 26.1
    :ensure t
    :bind (("M-=" . transient-dwim-dispatch))))

(leaf system-packages
  :doc "functions to manage system packages"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://gitlab.com/jabranham/system-packages"
  :added "2025-01-16"
  :emacs>= 24.3
  :ensure t
  :custom  ((system-packages-package-manager . 'apt)
            (system-packages-noconfirm . t)
            (system-packages-use-sudo . t)))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((user-full-name . "Karol Barski")
           ;; (user-login-name . "karolbarski")
           (truncate-lines . t)
           (menu-bar-mode . nil)
           (tool-bar-mode . nil)
           )
  ;; :config
  ;; (tool-bar-mode -1)
  )

(leaf delsel :hook (emacs-startup-hook .  delete-selection-mode))
(leaf goto-addr :hook (emacs-startup-hook . global-goto-address-mode))
(leaf mb-depth :hook (emacs-startup-hook . minibuffer-depth-indicate-mode))
(leaf sqlite
  :doc "Functions for interacting with sqlite3 databases"
  :tag "builtin"
  :added "2025-08-19"
  :custom
  (sql-sqlite-program . "sqlite3"))

(leaf scroll-bar
  :doc "window system-independent scroll bar support"
  :tag "builtin" "hardware"
  :added "2022-11-02"
  :custom ((scroll-bar-mode . nil)
           (horizontal-scroll-bar-mode . nil)
           )
  ;; :config
  ;; (horizontal-scroll-bar-mode -1)
  ;; (scroll-bar-mode -1)
  )

;; a few more useful configurations...
(leaf emacs
  :bind
  (("M-c" . capitalize-dwim)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("M-z" . zap-up-to-char)
   )
  :custom
  (kill-region-dwim . 'emacs-word)
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode . t)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers . t)

  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold . 3)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion . nil)

  ;; Hide commands in M-x which do not apply to the current
  ;; mode. Vertico and Corfu commands are hidden, since they are not used
  ;; via M-x. This setting is useful beyond Corfu and Vertico.
  (read-extended-command-predicate . #'command-completion-default-include-p)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))

  :config
  (message "Configure emacs fringe")
  ;; change truncation indicators
  (define-fringe-bitmap 'right-curly-arrow
    [#b10000000 #b10000000 #b01000000
     #b01000000 #b00100000 #b00100000
     #b00010000 #b00010000 #b00001000
     #b00001000 #b00000100 #b00000100])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000100 #b00000100 #b00001000
     #b00001000 #b00010000 #b00010000
     #b00100000 #b00100000 #b01000000
     #b01000000 #b10000000 #b10000000])
  )

(leaf frame
  ;;--------------------------------------------------------------------------------
  ;; Default frame parameters
  ;;--------------------------------------------------------------------------------
  :doc "multi-frame management independent of window systems"
  :tag "builtin" "internal"
  :added "2022-11-01"
  ;;   :pre-setq
  ;;   (kb/frame-config . '(;; (top . 1)
  ;;                        ;; (left . 1)
  ;;                        ;; (fullscreen . maximized)
  ;;                        (menu-bar-lines . 0)       ; turn menus off
  ;;                        (tool-bar-lines . 0)       ; disable toolbar
  ;;                        (scroll-bar-width . 10)
  ;;                        (vertical-scroll-bars . 'right)
  ;;                        ;; (background-mode . dark)
  ;;                        (font . "Hack")))
  ;;   :setq ((default-frame-alist . kb/frame-config)
  ;;          ;; (initial-frame-alist . kb/frame-config)
  ;;          )
  ;;   :custom (
  ;;            (blink-cursor-mode . nil)
  ;;            ;; turn off blinking cursor
  ;;            (blink-cursor-blinks . 3)
  ;;            (blink-cursor-delay . 1)
  ;;            )
  :push ((default-frame-alist . '(font . "Hack")))
  :config
  (blink-cursor-mode -1)
  ;;   ;; (mapc 'frame-set-background-mode (frame-list))
  ;;   ;; (fullscreen-restore . fullheight)
  ;;   ;; (fullscreen . fullboth)
  )

;; (leaf emacs-gc-stats
;;   :doc "Collect Emacs GC statistics"
;;   :req "emacs-25.1"
;;   :tag "emacs>=25.1"
;;   :url "https://git.sr.ht/~yantar92/emacs-gc-stats"
;;   :added "2023-06-13"
;;   :emacs>= 25.1
;;   :ensure t
;;   :mode emacs-gc-stats-mode
;;   :require emacs-gc-stats
;;   :config
;;   ;; (setq emacs-gc-stats-gc-defaults 'emacs-defaults) ; optional
;;   ;; (setq gc-cons-threshold (* 800000 (seq-random-elt '(1 2 4 8 16 32 64 128))))
;;   (emacs-gc-stats-mode +1)
;;   )

(leaf battery
  :doc "display battery status information"
  :tag "builtin"
  :added "2023-02-02"
  :config
  (display-battery-mode))

(leaf so-long
  :doc "Say farewell to performance problems with minified code."
  :tag "builtin"
  :added "2025-07-18"
  :custom
  (so-long-threshold . 5000)
  :config
  (nconc so-long-minor-modes
         '( spell-fu-mode
            eldoc-mode
            highlight-numbers-mode
            highlight-indent-guides-mode
            hl-fill-column-mode
            line-reminder-mode
            page-break-lines-mode
            tree-sitter-mode
            ts-fold-mode ts-fold-indicators-mode
            lsp-mode eglot--managed-mode
            whitespace-cleanup-mode))
  :hook (server-after-make-frame-hook . global-so-long-mode))

;; (leaf auto-compile
;;   :ensure t
;;   :require t
;;   :config
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;; MuLe commands
;; (leaf mule-cmds
;;   :doc "commands for multilingual environment"
;;   :tag "builtin" "i18n" "mule"
;;   :added "2022-12-08"
;;   :config
;;   (prefer-coding-system 'utf-8)
;;   (set-default-coding-systems 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (set-keyboard-coding-system 'utf-8))


;;--------------------------------------------------------------------------------
;; My emacs config
;;--------------------------------------------------------------------------------
;; (help-char "? M-?")
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(keymap-set help-map "?" 'describe-key-briefly)

;; ("C-h" . 'delete-backward-char)
;; ("C-?" . 'delete-char)
;; ("ESC C-h" . 'backward-kill-word)
;; ("ESC C-?" . 'kill-word)
;; ("<f1>" . 'help-command)
;; ("ESC ?" . 'help-command)
;; ("ESC ? F" . 'view-emacs-FAQ)

;;--------------------------------------------------------------------------------
;; Emacs (simple) config
;;--------------------------------------------------------------------------------
(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :custom ((global-mark-ring-max . 5000)         ; increase mark ring to contains 5000 entries
           (mark-ring-max . 5000)                ; increase kill ring to contains 5000 entries
           (kill-ring-max . 5000) ; increase kill-ring capacity
           (kill-whole-line . t)
           (transient-mark-mode . t)
           (indent-tabs-mode . nil)
           )
  :bind (("C-;" . kill-whole-line)
         ("C-j" . kb/join-line))
  :config
  ;; Join lines as in Vim
  (defun kb/join-line()
    "Join current and next line.

     Remove tralinig spaces leaving only one.  Similar to Vim Ctrl-j."
    (interactive)
    (join-line 'forward-line))

  :config
  (column-number-mode +1)
  (line-number-mode +1)
  (size-indication-mode t)
  (put 'set-goal-column 'disabled nil)
  )
;; remap C-H to backspace
;; (normal-erase-is-backspace-mode t)
;; remap C-H to backspace
(add-hook 'terminal-init-xterm-hook (lambda () (normal-erase-is-backspace-mode t)))

(leaf window
  :doc "GNU Emacs window commands aside from those written in C"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :bind (("M-o" . other-window))
  :init
  (put 'other-window 'repeat-map nil))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :added "2022-11-01"
  ;; ;; According to the POSIX, a line is defined as "a sequence of zero or
  ;; ;; more non-newline characters followed by a terminating newline".
  ;; (require-final-newline . t)
  :custom ((large-file-warning-threshold . 100000000)
           (mode-require-final-newline . t)      ; add a newline to end of file)
           (make-backup-files . nil)
           )
  :config
  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist (append major-mode-remap-alist '((yaml-mode . yaml-ts-mode)
                                                                (bash-mode . bash-ts-mode)
                                                                (js2-mode . js-ts-mode)
                                                                (typescript-mode . typescript-ts-mode)
                                                                (json-mode . json-ts-mode)
                                                                (css-mode . css-ts-mode)
                                                                (python-mode . python-ts-mode)
                                                                (javascript-mode . js-ts-mode)))))

(leaf ffap
  :doc "find file (or url) at point"
  :tag "builtin"
  :added "2025-06-17"
  :init
  (ffap-bindings)
  :config
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

(leaf indent
  :doc "indentation commands for Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent . 'complete))

(leaf page-break-lines
  :doc "Display ^L page breaks as tidy horizontal lines"
  :req "emacs-25.1"
  :tag "faces" "convenience" "emacs>=25.1"
  :url "https://github.com/purcell/page-break-lines"
  :added "2025-01-16"
  :emacs>= 25.1
  :ensure t
  :blackout t
  :hook (emacs-startup-hook . global-page-break-lines-mode)
  ;; :config (global-page-break-lines-mode)
  )

(leaf help
  :doc "help commands for Emacs"
  :tag "builtin" "internal" "help"
  :added "2022-11-01"
  :custom ((temp-buffer-resize-mode . t))
  )
;; image
(leaf image-file
  :doc "support for visiting image files"
  :tag "builtin"
  :added "2025-01-16"
  :custom
  (auto-image-file-mode . 1))
;; time

(leaf time
  :doc "display time, load and mail indicator in mode line of Emacs"
  :tag "builtin"
  :added "2023-01-31"
  :custom ((display-time-format . "%H:%M %d/%m/%Y")
           (display-time-24hr-format . t)
           (display-time-day-and-date . t)
           (display-time-default-load-average . nil) ; this information is useless for most
           )
  :config
  (display-time)
  )

(leaf font-lock
  ;; kolorowanie składni
  :doc "Electric font lock mode"
  :tag "builtin" "faces" "languages"
  :added "2022-11-01"
  :custom ((font-lock-maximum-decoration . t))
  :config (global-font-lock-mode t))

(leaf smerge-mode
  :doc "Minor mode to resolve diff3 conflicts"
  :tag "builtin"
  :added "2022-11-01"
  :init (setq-default smerge-command-prefix (kbd "C-c v"))
  ;; :custom (smerge-command-prefix . "C-c v")
  )

(defun kb/set-buffer-eol-unix ()
  "Set current buffer EOL type to unix."
  (interactive)
  (set-buffer-file-eol-type 'unix))
(defun kb/set-buffer-eol-dos ()
  "Set current buffer EOL type to dos."
  (interactive)
  (set-buffer-file-eol-type 'dos))
(defun kb/set-buffer-eol-mac ()
  "Set current buffer EOL type to mac."
  (interactive)
  (set-buffer-file-eol-type 'mac))

(defun kb/insert-random-number
    (&optional arg)
  "Insert a random number.   Use prefix argument ARG to specify the range.
If called with a prefix argument, prompts for MIN and MAX values."
  (interactive "P")
  (let ((min 0)
        (max 999))
    (when arg
      (setq min (read-number "Min value: " 0)
            max (read-number "Max value: " 999)))
    (insert (number-to-string (+ min (random (- max min)))))))

(global-set-key (kbd "C-c r") 'kb/insert-random-number)

(define-key global-map (kbd "C-a") 'prelude-move-beginning-of-line)
;;  (define-key global-map (kbd "C-c i") 'indent-region-or-buffer)
;; (define-key global-map (kbd "M-o") 'prelude-smart-open-line)
;; (define-key global-map (kbd "%") 'match-parenthesis) ;; % key on paren moves cursor to matching paren
(define-key global-map (kbd "C-c T") 'kb/delete-trailing-whitespaces-and-untabify)
(define-key global-map (kbd "C-c u") 'kb/set-buffer-eol-unix)
;; (define-key global-map (kbd "C-c d") 'kb/set-buffer-eol-dos)
;; (define-key global-map (kbd "C-c m") 'kb/set-buffer-eol-mac)
;; TODO: Temporary disable
;; (define-key global-map (kbd "C-c C-d") 'kb/insert-date-time)

(leaf insert-time-string
  :bind (("C-c C-!" . insert-time-string))
  )

(defun kb/update-env (fn)
  "Update environment variables reading FN file.

To prepare FN file issue following command in session of which variables
should be imported.
`printenv -o > ~/env.txt'"
  (interactive "fEnvironment file:")
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(leaf kill-file-path
  :doc "Copy file name into kill ring"
  :req "emacs-26"
  :tag "files" "emacs>=26"
  :url "https://github.com/chyla/kill-file-path/kill-file-path.el"
  :added "2024-02-19"
  :emacs>= 26
  :ensure t
  ;; :bind ("C-c f" . kill-file-path)
  )

(leaf ascii-table
  :doc "Interactive ASCII table"
  :req "emacs-24.3"
  :tag "tools" "help" "emacs>=24.3"
  :url "https://github.com/lassik/emacs-ascii-table"
  :added "2025-06-11"
  :emacs>= 24.3
  :ensure t)

;; (when (not (package-installed-p 'kill-file-path))
;;   (defun kb/filename ()
;;     "Copy the full path of the current buffer."
;;     (interactive)
;;     (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))
;;   (define-key global-map (kbd "C-c f") 'kb/filename)
;;   )

(leaf kill-or-bury-alive
  :doc "Precise control over buffer killing"
  :req "emacs-24.4"
  :tag "convenience" "emacs>=24.4"
  :url "https://github.com/mrkkrp/kill-or-bury-alive"
  :added "2024-02-19"
  :emacs>= 24.4
  :ensure t
  :bind
  (([remap kill-buffer] . #'kill-or-bury-alive)
   ;; ("C-c p" . #'kill-or-bury-alive-purge-buffers)
   ))

;; I know that string is in my Emacs somewhere!
;; (require 'cl)
(defcustom kb/search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list
  :group 'kb-config)
;; (require 'grep)
;; (defun kb/search-all-buffers (regexp prefix)
;;   "Searches file-visiting buffers for occurence of REGEXP.  With
;; prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
;; searches all buffers."
;;   (interactive (list (grep-read-regexp)
;;                      current-prefix-arg))
;;   (message "Regexp is %s; prefix is %s" regexp prefix)
;;   (multi-occur
;;    (if (member prefix '(4 (4)))
;;        (buffer-list)
;;      (remove-if
;;       (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) kb/search-all-buffers-ignored-files))
;;       (remove-if-not 'buffer-file-name (buffer-list))))
;;    regexp))

;; (global-set-key [f7] 'search-all-buffers)
;; (define-key global-map (kbd "") 'kb/update-env)

(leaf rect
  ;; string-insert-rectangle is useful but not binded to any key by default
  :bind (("C-x r l" . 'rectangle-number-lines))
  ;; (define-key global-map (kbd "C-x r a") 'string-insert-rectangle) ;; use string-rectange instead "C-x r t"
  )

(leaf whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :tag "builtin"
  :added "2022-11-01"
  :custom (
           ;; (setq whitespace-style '(face trailing lines-tail newline empty indentation big-indent space-before-tab))
           (whitespace-style . '(newline-mark newline))
           (whitespace-line-column . nil)
           (whitespace-display-mappings . '((space-mark 32 [183] [46])
                                            (newline-mark 10 [9166 10])
                                            (tab-mark 9 [9654 9] [92 9]))))
  :bind (("C-c w w" . whitespace-mode))
  :custom-face
  (whitespace-space . '((t (:inherit whitespace-space :foreground "DimGrey" :background nil))))
  (whitespace-newline . '((t (:inherit whitespace-newline :foreground "DimGrey" :background nil))))
  (whitespace-indentation . '((t (:inherit whitespace-indentation :foreground "DimGrey" :background nil))))

  :config
  (global-whitespace-mode 1)
  )

;; Set Theme depending if emacs frame is inside TTY o GUI
;;--------------------------------------------------------------------------------
(leaf solarized-theme
  :doc "The Solarized color theme"
  :req "emacs-24.1"
  :tag "solarized" "themes" "convenience" "emacs>=24.1"
  :url "http://github.com/bbatsov/solarized-emacs"
  :added "2022-11-04"
  ;; :emacs>= 24.1
  :ensure t)
(leaf solaire-mode
  :doc "make certain buffers grossly incandescent"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "faces" "buffer" "window" "bright" "dim" "emacs>=25.1"
  :url "https://github.com/hlissner/emacs-solaire-mode"
  :added "2023-02-14"
  :emacs>= 25.1
  :ensure t
  :config
  (solaire-global-mode))
(leaf sexy-theme
  :doc "Sexy color theme"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :url "http://github.com/bgcicca/sexy-theme.el"
  :added "2025-03-11"
  :emacs>= 24.1
  :ensure t)
(leaf sixcolors-theme
  :doc "Just another theme"
  :req "emacs-27.1"
  :tag "colors" "faces" "emacs>=27.1"
  :url "https://github.com/mastro35/sixcolors-theme"
  :added "2025-03-11"
  :emacs>= 27.1
  :ensure t)
(leaf rg-themes
  :doc "The rg theme collection"
  :req "emacs-25.1"
  :tag "faces" "emacs>=25.1"
  :url "https://github.com/raegnald/rg-themes"
  :added "2025-03-24"
  :emacs>= 25.1
  :ensure t)

(leaf zerodark-theme
  :doc "A dark, medium contrast theme for Emacs."
  :req "all-the-icons-2.0.0"
  :tag "themes"
  :url "https://github.com/NicolasPetton/zerodark-theme"
  :added "2025-06-16"
  :ensure t
  :config
  (defun kb/zerodark-theme-enabled (theme)
    (if (eq theme 'zerodark)
        (let ((background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
              (class '((class color) (min-colors 89)))
              (green (if (true-color-p) "#98be65" "#87af5f"))
              (orange (if (true-color-p) "#da8548" "#d7875f"))
              (purple (if (true-color-p) "#c678dd" "#d787d7")))
          (custom-theme-set-faces
           'zerodark
           `(vertico-current
             ((,class (:background
                       ,background-purple
                       :weight bold
                       :foreground ,purple))))
           `(prescient-primary-highlight ((,class (:foreground ,orange))))
           `(prescient-secondary-highlight ((,class (:foreground ,green))))
           `(completions-common-part nil))

          (dolist (face '(outline-1
                          outline-2
                          outline-3))
            (set-face-attribute face nil :height 1.0)))
      ))
  (add-hook 'enable-theme-functions #'kb/zerodark-theme-enabled))

(leaf doom-modeline
  :disabled t
  :hook window-setup-hook
  :ensure t
  :custom ((doom-modeline-hud . t)
           (doom-modeline-buffer-file-name-style . 'truncate-with-project)
           ;; (setq doom-modeline-enable-word-count nil)
           (doom-modeline-buffer-encoding . 'nondefault)
           (doom-modeline-default-coding-system . 'utf-8)
           (doom-modeline-default-eol-type . 0)
           (doom-modeline-vcs-max-length . 24)
           (doom-modeline-battery . t)
           (doom-modeline-indent-info . t)
           (doom-modeline-checker-simple-format . nil)
           (mode-line-right-align-edge . 'right-fringe)))

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library. Required by doom-modeline"
  :req "emacs-24.3"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :added "2023-06-15"
  :emacs>= 24.3
  :ensure t
  :config
  (setq nerd-icons-fonts-subdirectory (expand-file-name (concat user-emacs-directory "fonts/")))
  (unless (file-exists-p (expand-file-name (concat nerd-icons-fonts-subdirectory (car nerd-icons-font-names))))
    (nerd-icons-install-fonts t))
  )

(leaf nerd-icons-ibuffer
  :doc "Display nerd icons in ibuffer"
  :req "emacs-24.3" "nerd-icons-0.0.1"
  :tag "ibuffer" "icons" "convenience" "emacs>=24.3"
  :url "https://github.com/seagle0128/nerd-icons-ibuffer"
  :added "2023-06-15"
  :emacs>= 24.3
  :ensure t
  :hook ibuffer-mode-hook
  :custom
  (nerd-icons-ibuffer-icon . t)
  (nerd-icons-ibuffer-color-icon . t)
  (nerd-icons-ibuffer-icon-size . 1.0)
  (nerd-icons-ibuffer-human-readable-size . t)
  ;; :config
  ;; A list of ways to display buffer lines with `nerd-icons'.
  ;; See `ibuffer-formats' for details.
  ;; nerd-icons-ibuffer-formats
  )

(leaf nerd-icons-corfu
  :doc "Icons for Corfu via nerd-icons"
  :req "emacs-27.1" "nerd-icons-0.1.0"
  :tag "icons" "files" "convenience" "emacs>=27.1"
  :url "https://github.com/LuigiPiucco/nerd-icons-corfu"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf nerd-icons-completion
  :doc "Add icons to completion candidates"
  :req "emacs-25.1" "nerd-icons-0.0.1" "compat-30"
  :tag "lisp" "emacs>=25.1"
  :url "https://github.com/rainstormstudio/nerd-icons-completion"
  :added "2025-07-01"
  :emacs>= 25.1
  :ensure t
  :after nerd-icons compat
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(leaf nerd-icons-grep
  :doc "Add nerd-icons to grep-mode"
  :req "emacs-30.1" "nerd-icons-0.0.1"
  :tag "icons" "grep" "tools" "emacs>=30.1"
  :url "https://github.com/hron/nerd-icons-grep"
  :added "2025-07-23"
  :emacs>= 30.1
  :ensure t
  :after nerd-icons
  :init
  (nerd-icons-grep-mode)
  :custom
  ;; This setting is a pre-requirement, so an icon can be displayed near each
  ;; heading
  (grep-use-headings . t))

(leaf nerd-icons-xref
  :doc "Add nerd-icons to xref buffers"
  :req "emacs-30.1" "nerd-icons-0.0.1" "xref-1.0.4"
  :tag "icons" "xref" "tools" "emacs>=30.1"
  :url "https://github.com/hron/nerd-icons-xref"
  :added "2025-07-23"
  :emacs>= 30.1
  :ensure t
  :hook (xref--xref-buffer-mode-hook . nerd-icons-xref-mode)
  :after nerd-icons xref)

(leaf kind-icon
  :ensure t
  :when (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-blend-background . t)
  (kind-icon-default-face . 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf circadian
  :doc "Theme-switching based on daytime."
  :req "emacs-27.2"
  :tag "themes" "emacs>=27.2"
  :url "https://github.com/GuidoSchmidt/circadian"
  :added "2025-06-11"
  :emacs>= 27.2
  :ensure t
  :hook ((emacs-startup-hook . circadian-setup)
         (circadian-after-load-theme-hook .
                                          (lambda (theme)
                                            ;; Line numbers appearance
                                            (setq linum-format 'linum-format-func)
                                            ;; Cursor
                                            (set-default 'cursor-type 'box)
                                            (set-cursor-color "#F52503"))))
  :custom
  (calendar-latitude . 53.51)
  (calendar-longitude . 14.57)
  (circadian-themes . '((:sunrise . leuven-dark)
                        (:sunset . rg-themes-somnus)
                        ("8:00" . tango-dark)
                        ("8:15" . zerodark)
                        ("15:00" . (modus-vivendi
                                    sixcolors
                                    sexy
                                    rg-themes-purpurina))
                        ("15:15" . wombat)
                        ("17:00" . sixcolors)
                        ("21:30" . rg-themes-cappuccino-noir)))
  )

(leaf treemacs-nerd-icons
  :doc "Emacs Nerd Font Icons theme for treemacs"
  :req "emacs-24.3" "nerd-icons-0.0.1" "treemacs-0.0"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/treemacs-nerd-icons"
  :added "2023-06-15"
  :emacs>= 24.3
  :ensure t
  :defun treemacs-load-theme
  :after nerd-icons treemacs
  :require t
  :config
  (treemacs-load-theme "nerd-icons"))

;;--------------------------------------------------------------------------------
(leaf custom
  :doc "tools for declaring and initializing options"
  :tag "builtin" "faces" "help"
  :added "2022-11-04"
  :custom
  (custom-safe-themes . t))

;;--------------------------------------------------------------------------------
;; Programming modes
;;--------------------------------------------------------------------------------

(defconst kb/c-style
  '("linux"
    (fill-column . 100)
    (c-basic-offset . 4)
    (tab-width . 8)
    (indent-tabs-mode . t))
  "Style used for C source editing.")

(defun kb/c-mode-hook ()
  "My style used while editing C sources."
  (c-add-style "kb/c-style" kb/c-style t)
  (turn-on-auto-fill))

(defconst kb/c++-style
  '(
    (fill-column . 120)
    (c-basic-offset . 4)
    (tab-width . 4)
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((innamespace . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont . +)))
    (c-hanging-braces-alist . ((brace-list-open . before)
                               (brace-entry-open . before)
                               (substatement-open . before)
                               ;; (namespace-open . before)
                               ))
    )
  "Style used for C++ source editing.")

(defun kb/c++-setup-symbol-compose ()
  "Define additional symbol composition rules for C++ mode."
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("->" . ?→) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)
  ;; (push '("&&" . ?∧) prettify-symbols-alist)
  ;; (push '("||" . ?∨) prettify-symbols-alist)
  ;; (push '("^" . ?⊻) prettify-symbols-alist)
  )

(defun kb/c++-mode-hook ()
  "My style used while editing C++ sources."
  (c-add-style "kb/c++-style" kb/c++-style t)
  (auto-fill-mode)
  (display-fill-column-indicator-mode)
  (kb/c++-setup-symbol-compose))

(leaf cc-mode
  :doc "old c-mode and c++-mode config being replaced by c-ts-mode config below"
  :disabled t
  :tag "builtin"
  :added "2025-08-20"
  :config
  :hook (c-mode-hook . 'kb/c-mode-hook)
  :hook (c++-mode-hook . 'kb/c++-mode-hook)
  :config
  (defun kb/cc-compile-command-hook ()
    "Compile C/C++ files with gcc if makefile doesn't exist."
    (unless (or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format "%s -c -o %s.o %s %s %s"
                     (or (getenv "CC") "g++")
                     (file-name-sans-extension file)
                     (or (getenv "CPPFLAGS") "-DDEBUG=9")
                     (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                     file)))))

  ;; (add-hook 'c++-mode 'kb/cc-compile-command-hook)
  ;; (font-lock-add-keywords 'c-mode '("\\<\\(and\\|or\\|not\\)\\>"))
  )
(leaf c-ts-mode
  :doc "tree-sitter support for C and C++"
  :tag "builtin"
  :added "2025-08-20"
  :custom ((c-ts-common-indent-offset . 4)
           (c-ts-mode-enable-doxygen . t)
           (c-ts-mode-indent-offset . 4))
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'auto-mode-alist
               '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
                 . c++-ts-mode))

  :config
  (c-ts-mode-set-global-style 'linux)
  (defun kb/c++-ts-mode-hook ()
    "My style used while editing C++ sources."
    (setopt fill-column 120)
    ;; (tab-width . 4)
    (indent-tabs-mode . nil)
    (display-fill-column-indicator-mode)
    (kb/c++-setup-symbol-compose)
    )
  :hook ((c-ts-mode-hook c++-ts-mode-hook) . kb/c++-ts-mode-hook)
)

(leaf hl-todo
  :doc "Highlight TODO and similar keywords"
  :req "emacs-25.1" "compat-28.1.1.0"
  :tag "convenience" "emacs>=25.1"
  :url "https://github.com/tarsius/hl-todo"
  :added "2022-11-02"
  :emacs>= 25.1
  :ensure t
  ;; :bind ((:hl-todo-mode-map
  ;;         ("C-c t p" . #'hl-todo-previous)
  ;;         ("C-c t n" . #'hl-todo-next)
  ;;         ("C-c t o" . #'hl-todo-occur)
  ;;         ("C-c t i" . #'hl-todo-insert)))
  :hook prog-mode-hook
  :config
  (add-to-list 'hl-todo-keyword-faces '("NOCOMMIT" . "#ff00ff"))
  (global-hl-todo-mode))

(leaf hl-printf
  :doc "Minor mode for highlighting \"printf\" format specifiers"
  :req "emacs-24.1" "compat-29.1.0.1"
  :tag "faces" "c" "emacs>=24.1"
  :url "https://github.com/8dcc/hl-printf.el"
  :added "2025-01-15"
  :emacs>= 24.1
  :ensure t
  :after compat)

;;--------------------------------------------------------------------------------
;; GDB
;;--------------------------------------------------------------------------------
(leaf gdb-mi
  :doc "User Interface for running GDB"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((gdb-show-main . t) ;; Non-nil means display source file containing the main routine at startup
           )
  ;; :setq (gdb-many-windows . t)   ;; use gdb-many-windows by default
  )

(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :added "2022-11-01"
  :custom (display-line-numbers-width-start . t)
  :hook ((emacs-lisp-mode-hook
          lisp-mode-hook
          c-mode-common-hook
          prog-mode-hook) . display-line-numbers-mode)
  ;; display-line-numbers
  ;; (setq-default display-line-numbers-type 'relative)
  )

;; Show symbols as composed characters (e.g. lambda -> λ)
(leaf prog-mode
  :doc "Generic major mode for programming"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :config (global-prettify-symbols-mode t))

(leaf diff-mode
  :hook (diff-mode-hook . kb/whitespace-diff-setup)
  :config
  (defun kb/whitespace-diff-setup ()
    "Enable whitespace mode for diff files."
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
    (whitespace-mode 1))
  )

(leaf c-mode
  :init
  (defun kb/whitespace-progmode-setup ()
    "Enable whitespace mode for programming modes."
    (setq-local whitespace-style
                '(face
                  trailing
                  lines-tail
                  newline
                  empty
                  indentation
                  space-after-tab::tab
                  ;; space-after-tab
                  ;; big-indent
                  space-before-tab::tab
                  ;; space-before-tab
                  tab-mark
                  newline-mark
                  ))
    )
  :hook (c-mode-common-hook . kb/whitespace-progmode-setup)
  )

;; Highlight uncommitted changes
(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :req "cl-lib-0.2" "emacs-26.1"
  :tag "diff" "vc" "emacs>=26.1"
  :url "https://github.com/dgutov/diff-hl"
  :added "2025-03-27"
  :emacs>= 26.1
  :ensure t
  :hook (((prog-mode-hook vc-dir-mode-hook find-file-hook) . turn-on-diff-hl-mode)
          (dired-mode-hook . diff-hl-dired-mode)
          (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom ((diff-hl-side . 'right)
           (diff-hl-draw-borders . nil)
           ;; PERF: Slightly more conservative delay before updating the diff
           (diff-hl-flydiff-delay . 0.5)  ; default: 0.3
           ;; PERF: don't block Emacs when updating vc gutter
           ;;
           ;; NOTE: Async feature is buggy for now.
           (diff-hl-update-async . nil)
           ;; UX: get realtime feedback in diffs after staging/unstaging hunks
           (diff-hl-show-staged-changes . nil))
  :config
  (global-diff-hl-amend-mode 1))

;; vc-hooks
(leaf vc
  :doc "drive a version-control system from within Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((vc-follow-symlinks . t)
           (vc-handled-backends . '(git svn))))

(leaf vc-git
  :doc "VC backend for the git version control system"
  :tag "builtin" "tools" "vc"
  :added "2025-07-23"
  :custom
  ;; A slightly faster algorithm for diffing
  (vc-git-diff-switches . '("--histogram")))

;; subword-mode
(leaf subword
  :doc "Handling capitalized subwords in a nomenclature"
  :tag "builtin"
  :added "2022-11-02"
  :hook c-mode-common-hook)

;; compile
(defun kb/last-compilation-buffer ()
  "Display last compilation buffer in current window."
  (interactive)
  (if (buffer-live-p next-error-last-buffer)
      (set-window-buffer (get-buffer-window) compilation-last-buffer)
    (message "Last compilation buffer is killed.")))
(define-key global-map (kbd "C-x c") 'kb/last-compilation-buffer)

(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :added "2025-02-04"
  ;; :ensure tramp
  ;; :pin "gnu"
  :custom
  ;; (setq vc-ignore-dir-regexp .
  ;;                       '(format "\\(%s\\)\\|\\(%s\\)"
  ;;                                vc-ignore-dir-regexp
  ;;                                tramp-file-name-regexp))
  ;; Tips to speed up connections
  (tramp-verbose . 0)
  (tramp-chunksize . 2000)
  (tramp-use-ssh-controlmaster-options . nil)
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  ;; (add-to-list 'tramp-connection-properties
  ;;              (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
  ;;                    "direct-async-process" t))
  )

;;--------------------------------------------------------------------------------
;; iBuffer
;;--------------------------------------------------------------------------------
(leaf ibuffer
  :doc "operate on buffers like dired"
  :tag "builtin"
  :added "2022-11-01"
  :bind (("C-x C-b" . ibuffer))
  :custom (ibuffer-saved-filter-groups .
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
                                                     (filename . ".h")
                                                     (filename . ".cpp")
                                                     (filename . ".hpp")
                                                     (filename . ".java")
                                                     (filename . ".properties")
                                                     (filename . ".gradle")
                                                     (filename . ".am")
                                                     (mode . yaml-ts-mode)
                                                     (mode . yaml-mode)
                                                     (mode . yang-mode)
                                                     (mode . protobuf-mode))
                                           )
                                          ("Text" (or (filename . ".csv")
                                                      (filename . ".tsv")
                                                      (filename . ".txt")
                                                      (filename . ".log")
                                                      (filename . ".json")
                                                      (filename . ".md")))
                                          ("Emacs" (or (name . "^\\*scratch\\*$")
                                                       (name . "^\\*Messages\\*$")))
                                          ("Gnus" (or (mode . message-mode)
                                                      (mode . bbdb-mode)
                                                      (mode . mail-mode)
                                                      (mode . gnus-group-mode)
                                                      (mode . gnus-summary-mode)
                                                      (mode . gnus-article-mode)
                                                      (name . "^\\.bbdb$")
                                                      (name . "^\\.newsrc-dribble")))))))

(leaf ibuffer-projectile
  :doc "Group ibuffer's list by projectile root"
  :req "projectile-0.11.0" "emacs-24.1"
  :tag "convenience" "emacs>=24.1"
  :url "https://github.com/purcell/ibuffer-projectile"
  :added "2022-11-01"
  :emacs>= 24.1
  :ensure t
  :after projectile
  :hook (ibuffer-mode-hook . (lambda ()
                               (ibuffer-projectile-set-filter-groups)
                               (unless (eq ibuffer-sorting-mode 'alphabetic)
                                 (ibuffer-do-sort-by-alphabetic))))
  :custom ((ibuffer-formats .
                            '((mark modified read-only " "
                                    (name 18 18 :left :elide)
                                    " "
                                    (size 9 -1 :right)
                                    " "
                                    (mode 16 16 :left :elide)
                                    " "
                                    project-relative-file)))
           )
  )
(leaf ibuffer-vc :ensure t)


;;--------------------------------------------------------------------------------
;; Additional packages
;;--------------------------------------------------------------------------------
(leaf comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2)
  )
;; (leaf banner-comment
;;   :ensure t
;;   :bind ("M-'" . banner-comment)
;;   )

(leaf rainbow-delimiters
  :ensure t
  :hook prog-mode-hook)

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :added "2025-07-04"
  :ensure t
  :config (rainbow-mode))

(leaf anzu
  :doc "Show number of matches in mode-line while searching"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/emacsorphanage/anzu"
  :added "2022-10-28"
  :emacs>= 25.1
  :ensure t
  :hook (emacs-startup-hook . global-anzu-mode)
  :custom ((anzu-mode-lighter . "")
           (anzu-deactivate-region . t)
           (anzu-search-threshold . 1000)
           (anzu-replace-threshold . 50)
           (anzu-replace-to-string-separator . " => "))
  :bind ((([remap query-replace] . #'anzu-query-replace)
          ([remap query-replace-regexp] . #'anzu-query-replace-regexp))
         (:isearch-mode-map
          ([remap isearch-query-replace] . #'anzu-query-replace)
          ([remap isearch-query-replace-regexp] . #'anzu-query-replace-regexp)))
  ;; :custom-face
  ;; (anzu-mode-line . '((t (:inherit anzu-mode-line :foreground "yellow" :weight "bold"))))
  ;; :config
  ;; (global-anzu-mode +1)
  )

(leaf golden-ratio
  :ensure t
  :blackout t
  :config (golden-ratio-mode))

(leaf editorconfig
  :ensure t
  :config (editorconfig-mode 1))
(leaf editorconfig-generate
  :doc "Generate .editorconfig"
  :req "emacs-24"
  :tag "tools" "emacs>=24"
  :url "https://github.com/10sr/editorconfig-generate-el"
  :added "2025-01-28"
  :emacs>= 24
  :ensure t)

(defun kb/projectile-project-compilation (docker-image cmd)
  "Prepare projectile compilation CMD to be run in DOCKER-IMAGE."
  (let ((project-dir (expand-file-name projectile-project-compilation-dir))
        (docker-home-dir (expand-file-name "~/.hmi-build-home/"))
        (user-home-dir (expand-file-name "~")))
    (format "docker run --rm --user %s:%s -v %s:%s -w %s --mount type=bind,source=%s,target=%s --env MAKETHREADCOUNT=8 %s %s" (user-uid) (group-gid) docker-home-dir user-home-dir project-dir project-dir project-dir docker-image cmd)))

;; (message (kb/projectile-project-compilation "./build_linux64mgu22.sh"))

(defcustom kb/docker-executable 'docker
  "The executable to be used with docker-mode.

Based on config described in https://www.rahuljuliato.com/posts/emacs-docker-podman."
  :type '(choice
          (const :tag "docker" docker)
          (const :tag "podman" podman))
  :group 'kb-config)

(leaf docker
  :doc "Interface to Docker"
  :req "aio-1.0" "dash-2.19.1" "emacs-26.1" "s-1.13.0" "tablist-1.1" "transient-0.4.3"
  :tag "convenience" "filename" "emacs>=26.1"
  :url "https://github.com/Silex/docker.el"
  :added "2025-06-02"
  :emacs>= 26.1
  :ensure t
  :after aio tablist
  :bind ("C-c d" . docker)
  :config
  (pcase kb/docker-executable
    ('docker
     (setf docker-command "docker"
           docker-compose-command "docker-compose"
           docker-container-tramp-method "docker"))
    ('podman
     (setf docker-command "podman"
           docker-compose-command "podman-compose"
           docker-container-tramp-method "podman"))))

(leaf dockerfile-ts-mode
  :doc "tree-sitter support for Dockerfiles"
  :tag "builtin"
  :added "2025-08-20"
  :mode "Dockerfile\\'")

(leaf posframe
  :doc "Pop a posframe (just a frame) at point"
  :req "emacs-26.1"
  :tag "tooltip" "convenience" "emacs>=26.1"
  :url "https://github.com/tumashu/posframe"
  :added "2025-01-20"
  :emacs>= 26.1
  :ensure t)

(leaf stillness-mode
  :disabled t
  :doc "Prevent windows from jumping on minibuffer activation"
  :req "emacs-26.1" "dash-2.18.0"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/neeasade/stillness-mode.el"
  :added "2025-02-03"
  :emacs>= 26.1
  :ensure t)

(leaf flycheck
  :disabled t
  :doc "On-the-fly syntax checking"
  :blackout t
  :req "emacs-26.1"
  :tag "tools" "languages" "convenience" "emacs>=26.1"
  :url "https://www.flycheck.org"
  :added "2024-03-18"
  :emacs>= 26.1
  :ensure t
  :init (global-flycheck-mode)
  :custom ((flycheck-indication-mode . 'right-fringe)
           (flycheck-check-syntax-automatically . '(save mode-enabled))
           (flycheck-checker-error-threshold . 10000))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]
      ))
  )

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :tag "builtin"
  :added "2025-07-04"
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . scan-buf-next-region)
         ("M-g M-p" . scan-buf-previous-region)
         ;; (:flymake-repeatmap
         ;;  ("p" . scan-buf-previous-region)
         ;;  ("n" . scan-buf-next-region)
         ;;  ("M-p" . scan-buf-previous-region)
         ;;  ("M-n" . scan-buf-next-region))
         (:flymake-diagnostics-buffer-mode-map
          ("?" . flymake-show-diagnostic-here))
         (:flymake-project-diagnostics-mode-map
          ("?" . flymake-show-diagnostic-here))
         )
  :hook (prog-mode . flymake-mode)
  :config
  (defun flymake-show-diagnostic-here (pos &optional other-window)
    "Show the full diagnostic of this error.

Used to see multiline flymake errors"
    (interactive (list (point) t))
    (let* ((id (or (tabulated-list-get-id pos)
                   (user-error "Nothing at point")))
           (text (flymake-diagnostic-text (plist-get id :diagnostic))))
      (message text)))
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  )

(leaf flyover
  :doc "Display Flycheck and Flymake errors with overlays"
  :req "emacs-27.1" "flymake-1.0"
  :tag "flymake" "flycheck" "tools" "convenience" "emacs>=27.1"
  :url "https://github.com/konrad1977/flyover"
  :added "2025-07-13"
  :emacs>= 27.1
  :ensure t
  :after flymake
  :hook (flymake-mode-hook . flyover-mode)
  ;; :custom ((flyover-levels . '(error warning info))
  ;;          (flyover-use-theme-colors . t)
  ;;          (flyover-background-lightness . 45)
  ;;          (flyover-percent-darker . 40)
  ;;          (flyover-text-tint . 'lighter)
  ;;          (flyover-text-tint-percent . 50)
  ;;          (flyover-checkers . '(flymake ;; flycheck
  ;;                                        ))
  ;;          (flyover-debounce-interval . 0.2)
  ;;          (flyover-line-position-offset . 1)
  ;;          (flyover-wrap-messages . t)
  ;;          ;; (flyover-max-line-length . 80)
  ;;          (flyover-info-icon . "🛈")
  ;;          (flyover-warning-icon . "⚠")
  ;;          (flyover-error-icon . "🚫")
  ;;          (flyover-icon-left-padding . 0.9)
  ;;          (flyover-icon-right-padding . 0.9)
  ;;          (flyover-virtual-line-type . 'curved-dotted-arrow)
  ;;          (flyover-hide-checker-name . t)
  ;;          (flyover-show-at-eol . t)
  ;;          (flyover-hide-when-cursor-is-on-same-line . t)
  ;;          (flyover-show-virtual-line . t)
  ;;          )
  :config
  (custom-set-faces
   '(flyover-error
     ((t :background "#453246"
         :foreground "#ea8faa"
         :height 0.9
         :weight normal)))
   '(flyover-warning
     ((t :background "#331100"
         :foreground "#DCA561"
         :height 0.9
         :weight normal)))
   '(flyover-info
     ((t :background "#374243"
         :foreground "#a8e3a9"
         :height 0.9
         :weight normal)))))

(leaf flymake-shell
  :doc "A flymake syntax-checker for shell scripts"
  :req "flymake-easy-0.1"
  :url "https://github.com/purcell/flymake-shell"
  :added "2025-07-07"
  :ensure t
  :after flymake-easy
  :hook (sh-set-shell-hook . flymake-shell-load))

(leaf flymake-flycheck
  :disabled t
  :doc "Use flycheck checkers as flymake backends"
  :req "flycheck-31" "emacs-26.1"
  :tag "tools" "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/purcell/flymake-flycheck"
  :added "2025-07-04"
  :emacs>= 26.1
  :ensure t
  :after flymake
  :init
  (setopt flycheck-disabled-checkers '(python-mypy haskell-ghc haskell-hlint python-pycompile))
  :hook (flymake-mode-hook . flymake-flycheck-auto))

(leaf pretty-hydra
  :disabled t
  :doc "A macro for creating nice-looking hydras"
  :req "hydra-0.15.0" "s-1.12.0" "dash-2.18.0" "emacs-24" "compat-29.1.4.1"
  :tag "emacs>=24"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :added "2025-03-11"
  :emacs>= 24
  :ensure t
  :after hydra compat)
(leaf major-mode-hydra
  :disabled t
  :doc "Major mode keybindings managed by Hydra"
  :req "dash-2.18.0" "pretty-hydra-0.2.2" "emacs-25"
  :tag "emacs>=25"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :added "2025-03-11"
  :emacs>= 25
  :ensure t
  :after pretty-hydra
  :bind (("C-'" . #'major-mode-hydra))
  :config
  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (
      ;; ("d" describe-foo-at-point "thing-at-pt")
      ("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup")))))

(leaf flycheck-hydra
  :disabled t
  :when (package-installed-p 'flycheck)
  :after flycheck hydra
  :config
  (pretty-hydra-define hydra-flycheck-pretty (global-map "C-c ! j"
                                                         :pre (flycheck-list-errors)
                                                         :post (quit-windows-on "*Flycheck errors*")
                                                         :hint nil)
    ("Errors"
     (("f" flycheck-error-list-set-filter "Filter")
      ("j" flycheck-next-error "Next")
      ("k" flycheck-previous-error "Previous")
      ("gg" flycheck-first-error "First")
      ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
      ("q" nil)
      )))
  ;; (defhydra hydra-flycheck
  ;;   (global-map "C-c ! j"
  ;;               :pre (flycheck-list-errors)
  ;;               :post (quit-windows-on "*Flycheck errors*")
  ;;               :hint nil)
  ;;   "Errors"
  ;;   ("f" flycheck-error-list-set-filter "Filter")
  ;;   ("j" flycheck-next-error "Next")
  ;;   ("k" flycheck-previous-error "Previous")
  ;;   ("gg" flycheck-first-error "First")
  ;;   ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ;;   ("q" nil))
  )
(leaf flycheck-clang-analyzer
  :when (package-installed-p 'flycheck)
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(leaf flycheck-clang-tidy
  :when (package-installed-p 'flycheck)
  :doc "Flycheck syntax checker using clang-tidy"
  :req "flycheck-0.30"
  :tag "tools" "languages" "convenience"
  :url "https://github.com/ch1bo/flycheck-clang-tidy"
  :added "2025-05-26"
  :ensure t
  :after flycheck projectile
  :hook (flycheck-mode-hook . flycheck-clang-tidy-setup))
(leaf flycheck-projectile
  :when (package-installed-p 'flycheck)
  :doc "Project-wide errors"
  :req "emacs-25.1" "flycheck-31" "projectile-2.2"
  :tag "emacs>=25.1"
  :url "https://github.com/nbfalcon/flycheck-projectile"
  :added "2025-02-03"
  :emacs>= 25.1
  :after flycheck projectile
  :ensure t
  :require t)
(leaf flycheck-posframe
  :when (package-installed-p 'flycheck)
  :doc "Show flycheck error messages using posframe.el"
  :req "flycheck-0.24" "emacs-26" "posframe-0.7.0"
  :tag "emacs>=26"
  :url "https://github.com/alexmurray/flycheck-posframe"
  :added "2025-02-03"
  :emacs>= 26
  :after flycheck
  :ensure t
  :hook flycheck-mode-hook)

;; (advice-add 'ispell-pdict-save :after (lambda (_) (message "KB advice added") '((name . kukuryku))))

;;================================================================================
;; Spell checking
;;================================================================================
(leaf flycheck-aspell
  :doc "Aspell checker for flycheck"
  :req "flycheck-28.0" "emacs-25.1"
  :tag "aspell" "spell" "flycheck" "wp" "emacs>=25.1"
  :url "https://github.com/leotaku/flycheck-aspell"
  :added "2025-01-27"
  :emacs>= 25.1
  :ensure t
  :when (package-installed-p 'flycheck)
  :after flycheck
  :require t
  :advice ((:after ispell-pdict-save flycheck-maybe-recheck))
  :preface
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
  )

(leaf use-ttf
  :doc "Keep font consistency across different OSs."
  :req "emacs-26.1"
  :tag "ttf" "install" "font" "customize" "convenience" "emacs>=26.1"
  :url "https://github.com/jcs-elpa/use-ttf"
  :added "2025-06-23"
  :emacs>= 26.1
  :ensure t
  :hook (after-init-hook . use-ttf-set-default-font)
  :init
  (setq use-ttf-default-ttf-fonts
        (mapcar (lambda (file) (concat user-emacs-directory file))
                '("fonts/clacon.ttf"
                  "fonts/DejaVuSans.ttf"
                  "fonts/DejaVuSansMono.ttf"
                  "fonts/NFM.ttf"                      ; nerd-icons
                  "fonts/NotoSans-Regular.ttf"
                  "fonts/NotoSansSymbols-Regular.ttf"
                  "fonts/Quivira.otf"
                  "fonts/Symbola.otf"                  ; for unicode
                  "fonts/UbuntuMono-R.ttf"))
        use-ttf-default-ttf-font-name "Ubuntu Mono"))

(leaf unicode-fonts
  :disabled t
  :doc "Configure Unicode fonts"
  :req "font-utils-0.7.8" "ucs-utils-0.8.2" "list-utils-0.4.2" "persistent-soft-0.8.10" "pcache-0.3.1"
  :tag "interface" "wp" "frames" "faces" "i18n"
  :url "http://github.com/rolandwalker/unicode-fonts"
  :added "2025-03-27"
  :ensure t
  :after font-utils ucs-utils list-utils persistent-soft pcache
  ;; :config


  ;;         https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip
  ;;         http://www.quivira-font.com/files/Quivira.ttf   ; or Quivira.otf
  ;;         http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2
  ;;         https://github.com/googlei18n/noto-fonts/raw/master/hinted/NotoSans-Regular.ttf
  ;;         https://github.com/googlei18n/noto-fonts/raw/master/unhinted/NotoSansSymbols-Regular.ttf
  )

;; http://unifoundry.com/pub/unifont/unifont-14.0.01/font-builds/unifont-14.0.01.ttf
;; (defvar kb/fonts '((:url "http://unifoundry.com/pub/unifont/unifont-14.0.03/font-builds/"
;;                          :fonts "unifont-14.0.03.otf"
;;                          :method 'download)
;;                    (:url "https://github.com/source-foundry/Hack/releases/download/v3.003/"
;;                          :fonts "Hack-v3.003-ttf.tar.xz"
;;                          :archive "Hack-v3.003-ttf.tar.xz"
;;                          :method 'tarxz)
;;                    (:url "https://github.com/google/fonts/raw/main/ofl/cantarell/"
;;                          :fonts ("Cantarell-BoldOblique.ttf"
;;                                  "Cantarell-Bold.ttf"
;;                                  "Cantarell-Oblique.ttf"
;;                                  "Cantarell-Regular.ttf")
;;                          :metod 'download)
;;                    )
;;   "List of font urls which should be installed.")

;; (defun kb/install-fonts (&optional pfx)
;;   "Helper function to download and install recommended fonts based on OS.
;; When PFX is non-nil, ignore the prompt and just install.

;; This function is based on all-the-icons-install-fonts"
;;   (interactive "P")
;;   (when (or pfx (yes-or-no-p "This will download and install fonts, are you sure you want to do this?"))
;;     (let* ((font-dest (cond
;;                        ;; Default Linux install directories
;;                        ((member system-type '(gnu gnu/linux gnu/kfreebsd))
;;                         (concat (or (getenv "XDG_DATA_HOME")
;;                                     (concat (getenv "HOME") "/.local/share"))
;;                                 "/fonts/"))
;;                        ;; Default MacOS install directory
;;                        ((eq system-type 'darwin)
;;                         (concat (getenv "HOME") "/Library/Fonts/"))))
;;            (known-dest? (stringp font-dest))
;;            (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

;;       (unless (file-directory-p font-dest) (mkdir font-dest t))

;;       (mapc (lambda (font-url)
;;            (let* ((fonts-arg (plist-get kb/fonts :fonts))
;;                   (files (if (atom fonts-arg) (list fonts-arg) (fonts-arg))))
;;              (mapc (lambda (file)
;;                      (let* ((file-url (concat (plist-get font-url :url) file))
;;                             (file-dst (expand-file-name file font-dest))
;;                             )
;;                        (prin1 "file-url: ")
;;                        (prin1 file-url)
;;                        (prin1 " -> file-dst: ")
;;                        (print file-dst)
;;                        (url-copy-file file-url
;;                                     file-dst t)
;;                      )) files)))
;;          kb/fonts)
;;       (when known-dest?
;;         (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
;;         (shell-command-to-string (format "fc-cache -f -v")))
;;       (message "%s Successfully %s `all-the-icons' fonts to `%s'!"
;;                (all-the-icons-wicon "stars" :v-adjust 0.0)
;;                (if known-dest? "installed" "downloaded")
;;               font-dest))))

(leaf vundo
  :doc "Visual undo tree"
  :req "emacs-28.1"
  :tag "editing" "text" "undo" "emacs>=28.1"
  :url "https://github.com/casouri/vundo"
  :added "2025-01-15"
  :emacs>= 28.1
  :ensure t
  :custom ((vundo-roll-back-on-quit . t) ; t is default
           (vundo-glyph-alist . vundo-unicode-symbols))
  :defer-config
  (set-face-attribute 'vundo-default nil :family "Symbola")
  )

(leaf undo-fu-session
  :doc "Persistent undo, available between sessions"
  :req "emacs-28.1"
  :tag "convenience" "emacs>=28.1"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu-session"
  :added "2025-06-12"
  :emacs>= 28.1
  :ensure t
  ;; :bind (("C-x u"   . undo-only)
  ;;        ("C-/" . undo-only)
  ;;        ("C-?" . undo-redo)
  ;;        ("C-z"     . undo-only)
  ;;        ("C-S-z"   . undo-redo))
  ;; (global-unset-key (kbd "C-z"))
  ;; (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  ;; (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  :config
  (undo-fu-session-global-mode))

(leaf keycast
  :doc "Show current command and its binding"
  :req "emacs-28.1" "compat-30.0.2.0"
  :tag "multimedia" "emacs>=28.1"
  :url "https://github.com/tarsius/keycast"
  :added "2025-05-09"
  :emacs>= 28.1
  :ensure t
  :after compat)

(leaf afterglow
  :doc "Temporary Highlighting after Function Calls"
  :req "emacs-26.1"
  :tag "evil" "convenience" "line" "highlight" "emacs>=26.1"
  :url "https://github.com/ernstvanderlinden/emacs-afterglow"
  :added "2025-01-28"
  :emacs>= 26.1
  :ensure t
  :blackout t
  :require t
  :config
  ;; ;; Optional
  ;; (setq afterglow-default-duration 0.5)
  ;; ;; Optional
  ;; (setq afterglow-default-face 'hl-line)
  ;;
  (afterglow-add-triggers
   '(
     ;; (evil-previous-visual-line :thing line :width 5 :duration 0.2)
     ;; (evil-next-visual-line :thing line :width 5 :duration 0.2)
     (previous-line :thing line :duration 0.2)
     (next-line :thing line :duration 0.2)
     (eval-buffer :thing window :duration 0.2)
     (eval-defun :thing defun :duration 0.2)
     (eval-expression :thing sexp :duration 1)
     (eval-last-sexp :thing sexp :duration 1)

     ;; (my-function :thing my-region-function :duration 0.5 :face 'highlight)
     ))
  (afterglow-mode 1))

(leaf gnuplot
  :doc "Major-mode and interactive frontend for gnuplot"
  :req "emacs-24.3"
  :tag "plotting" "gnuplot" "data" "emacs>=24.3"
  :url "https://github.com/emacs-gnuplot/gnuplot"
  :added "2022-10-31"
  :emacs>= 24.3
  :ensure t)

(leaf org
  :doc "Outline-based notes management and organizer"
  :added "2022-10-31"
  :ensure t
  :commands (org-capture org-agenda)
  :hook ((org-mode-hook . kb/org-mode-setup)
         (org-mode-hook . kb/org-font-setup))
  :custom (
           (org-startup-indented . t)
           (org-startup-with-inline-images . t)
           (org-pretty-entities . t)
           (org-use-sub-superscripts . "{}")
           (org-hide-emphasis-markers . t)
           (org-hide-block-startup . t)
           (org-image-actual-width . '(300))

           (org-ellipsis . " ▾")
           (org-hide-leading-stars . t)
           (org-agenda-start-with-log-mode . t)
           (org-log-done . 'time)
           (org-log-into-drawer . t)
           (org-src-fontify-natively . t)
           (org-list-allow-alphabetical . t)

           (org-export-default-language . "en")
           (org-export-with-fixed-width . nil)
           (org-export-preserve-breaks . t)
           (org-export-with-properties . t)
           (org-export-with-section-numbers . nil)
           (org-export-with-smart-quotes . t)
           (org-export-with-drawers . nil)
           (org-export-with-todo-keywords . nil)
           (org-export-with-broken-links . t)
           (org-export-with-toc . nil)
           (org-export-with-smart-quotes . t)
           (org-export-date-timestamp-format . "%d %B %Y")
           (org-export-with-sub-superscripts . '{})
           (org-export-copy-to-kill-ring . 'if-interactive)
           (org-export-show-temporary-export-buffer . nil)

           ;; (setq org-src-fontify-natively t)
           ;; (setq org-export-with-smart-quotes nil)
           ;; (org-html-htmlize-output-type . nil)
           (org-html-table-default-attributes .
                                              '(:border "1" :rules "border" :frame "all"))
           (org-html-metadata-timestamp-format . "%A, %B %d, %Y")
           (org-html-postamble . nil)
           (org-todo-keywords .
                              '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                                (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
           )
  :config
  (defun kb/org-mode-setup ()
    (org-indent-mode 1)
    ;;(variable-pitch-mode 1)
    (visual-line-mode 1)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (eshell . t)
       (screen . t)
       (shell . t)
       (dot . t)
       (gnuplot . t)
       ))
    )

  (defun kb/org-font-setup ()
    "Function to setup font configuration for Org mode files.

This function is based on work of David Wilson.
"
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))


    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

    ;; (require 'color)
    ;; (set-face-attribute 'org-block nil :background
    ;;                     (color-darken-name
    ;;                      (face-attribute 'default :background) 3))

    ;; (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
    ;;                             ("python" (:background "#E5FFB8"))
    ;;                          ("cpp" (:background "grey5" :foreground "chartreuse"))
    ;;                          ("protobuf" (:background "grey5" :foreground "chartreuse"))
    ;;                          ))
    )

  (defun ews-org-insert-screenshot ()
    "Take a screenshot with ImageMagick and insert as an Org mode link."
    (interactive)
    (let ((filename (read-file-name "Enter filename for screenshot: " default-directory)))
      (unless (string-equal "png" (file-name-extension filename))
        (setq filename (concat (file-name-sans-extension filename) ".png")))
      (call-process-shell-command (format "import %s" filename))
      (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
      (insert (format "[[file:%s]]" filename))
      (org-redisplay-inline-images)))

  ;; (with-eval-after-load "org"
  ;;   (define-key org-mode-map (kbd "C-<print>") #'ews-org-insert-screenshot))
  (keymap-set org-mode-map "C-<print>" #'ews-org-insert-screenshot)
  ;; (keymap-set-after org-mode-map (kbd "C-<print>")  #'ews-org-insert-screenshot "org")
  )

(leaf org-appear
  :doc "Auto-toggle Org elements"
  :req "emacs-29.1" "org-9.3"
  :tag "emacs>=29.1"
  :url "https://github.com/awth13/org-appear"
  :added "2024-11-21"
  :emacs>= 29.1
  :ensure t
  :after org
  :hook org-mode-hook)

(leaf org-modern
  :doc "Modern looks for Org"
  :req "emacs-28.1" "compat-30"
  :tag "text" "hypermedia" "outlines" "emacs>=28.1"
  :url "https://github.com/minad/org-modern"
  :added "2024-11-21"
  :emacs>= 28.1
  :ensure t
  :after compat
  :hook org-mode-hook ;; global-org-modern-mode
  :custom
  (org-modern-keyword . nil)
  ;; (setq org-modern-checkbox . nil)
  (org-modern-table . nil)
  (org-modern-star . 'fold)
  (org-modern-fold-stars . '(("◉" . "○") ("◈" . "◇") ("*" .  "⁑") ("✳" . "⁂") ("▶" . "▼") ("▷" . "▽") ("⯈" . "⯆") ("▹" . "▿") ("▸" . "▾")))
  (org-modern-list . '((?* . "•")
                       (?+ . "➤")
                       (?- . "◦"))
                   ))

(leaf org-make-toc
  :doc "Automatic tables of contents for Org files"
  :req "emacs-26.1" "dash-2.12" "s-1.10.0" "org-9.0"
  :tag "convenience" "org" "emacs>=26.1"
  :url "http://github.com/alphapapa/org-make-toc"
  :added "2022-12-05"
  :emacs>= 26.1
  :ensure t
  :after org
  :hook org-mode-hook)

(leaf org-link-beautify
  :doc "Beautify Org Links"
  :req "emacs-29.1" "nerd-icons-0.0.1" "qrencode-1.3"
  :tag "hypermedia" "emacs>=29.1"
  :url "https://repo.or.cz/org-link-beautify.git"
  :added "2025-08-19"
  :emacs>= 29.1
  :ensure t
  :after nerd-icons qrencode)

(leaf polish-holidays
  :doc "Polish holidays"
  :req "emacs-24.1"
  :tag "calendar" "emacs>=24.1"
  :url "https://github.com/przemarbor/polish-holidays"
  :added "2025-08-19"
  :emacs>= 24.1
  :ensure t
  ;; :after holidays
  :hook (calendar-mode-hook . polish-holidays-set)
  :custom
  (polish-holidays-use-all-p . t)
  :config
  (polish-holidays-set))

(leaf htmlize
  :disabled t
  :doc "Convert buffer text and decorations to HTML."
  :tag "extensions" "hypermedia"
  :url "https://github.com/hniksic/emacs-htmlize"
  :added "2022-10-31"
  :ensure t)

(leaf ox-latex
  :doc "LaTeX Backend for Org Export Engine"
  :tag "builtin" "text" "calendar" "hypermedia" "outlines"
  :added "2024-08-05"
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process .
                         '("pdflatex -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions .
                                 (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
                                         "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
                                         "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
                                         "tex" "bcf"))))

(leaf ox-gfm
  :doc "Github Flavored Markdown Back-End for Org Export Engine"
  :tag "github" "markdown" "wp" "org"
  :added "2023-04-13"
  :ensure t
  :after ox
  :require t)

(leaf ox-reveal
  :doc "reveal.js Presentation Back-End for Org Export Engine"
  :req "org-8.3"
  :tag "presentation" "slideshow" "hypermedia" "outlines"
  :added "2022-10-31"
  :ensure t
  :after org
  :custom ((org-reveal-root . "https://cdn.jsdelivr.net/npm/reveal.js")
           (org-reveal-mathjax . t))
  :require t)

(leaf ox-jira
  :doc "JIRA Backend for Org Export Engine"
  :req "org-8.3"
  :tag "wp" "hypermedia" "outlines"
  :url "https://github.com/stig/ox-jira.el"
  :added "2022-10-31"
  :ensure t
  :after ox
  :require t)

(leaf ox-mediawiki
  :doc "Mediawiki Back-End for Org Export Engine"
  :req "cl-lib-0.5" "s-1.9.0"
  :tag "mediawiki" "wp" "org"
  :url "https://github.com/tomalexander/orgmode-mediawiki"
  :added "2022-10-31"
  :ensure t
  :require ox-mediawiki
  :after ox
  :require t)

(leaf ox-tiddly
  :doc "Org TiddlyWiki exporter"
  :req "org-8" "emacs-24.4"
  :tag "org" "emacs>=24.4"
  :url "https://github.com/dfeich/org8-wikiexporters"
  :added "2022-10-31"
  ;; :emacs>= 24.4
  :ensure t
  :after ox
  :require t)

(leaf ox-wk
  :doc "Wiki Back-End for Org Export Engine"
  :req "emacs-24.4" "org-8.3"
  :tag "wiki" "wp" "org" "emacs>=24.4"
  :url "https://github.com/w-vi/ox-wk.el"
  :added "2022-10-31"
  :ensure t
  :after ox
  :require t)

;; (require 'ox-confluence)

(leaf org-journal
  :doc "a simple org-mode based journaling mode"
  :req "emacs-25.1" "org-9.1"
  :tag "emacs>=25.1"
  :url "http://github.com/bastibe/org-journal"
  :added "2022-10-31"
  ;; :emacs>= 25.1
  :ensure t
  ;; :after org
  :commands (org-journal-new-entry)
  ;;  :init ()
  ;; :require org
  :custom ((org-journal-dir . "~/projects/journal/")
           (org-journal-file-format . "%Y%m%d.org"))
  :commands (org-journal-new-entry)
  ;; :bind-keymap* ("C-c C-j" . #'org-journal-new-entry)
  ;; :require org org-journal
  :bind ("C-c C-j" . 'org-journal-new-entry)
  )
;; (define-key global-map (kbd "C-c C-j") 'org-journal-new-entry)

(leaf verb
  :doc "Organize and send HTTP requests"
  :req "emacs-26.3"
  :tag "tools" "emacs>=26.3"
  :url "https://github.com/federicotdn/verb"
  :added "2023-11-09"
  :emacs>= 26.3
  :ensure t
  :after org
  ;; :bind-keymap (:org-mode-map ("C-c C-1" . verb-command-map))
  :config
  (add-to-list 'org-babel-load-languages '((verb . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; (define-key org-mode-map (kbd "C-c C-0") verb-command-map)
  (keymap-set org-mode-map "C-c C-0" verb-command-map)
  )

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-26.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=26.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :bind (:markdown-mode-map
         ("C-c C-e" . markdown-do))
  :config
  (when (not (executable-find "pandoc"))
    (message "**** Configure markdown-mode pandoc not found")))

(leaf markdown-preview-mode
  :doc "Markdown realtime preview minor mode"
  :req "emacs-24.4" "websocket-1.6" "markdown-mode-2.0" "cl-lib-0.5" "web-server-0.1.1"
  :tag "convenience" "gfm" "markdown" "emacs>=24.4"
  :url "https://github.com/ancane/markdown-preview-mode"
  :added "2025-06-26"
  :emacs>= 24.4
  :ensure t
  :after websocket markdown-mode web-server)

(leaf flymake-markdownlint
  :doc "Markdown linter with markdownlint"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/shaohme/flymake-markdownlint"
  :added "2025-07-07"
  :emacs>= 27.1
  :ensure t
  :hook (markdown-mode-hook . flymake-markdownlint-setup))

(leaf highlight-doxygen
  :disabled t
  :doc "Highlight Doxygen comments"
  :tag "faces"
  :url "https://github.com/Lindydancer/highlight-doxygen"
  :added "2022-10-31"
  :ensure t
  :hook c-mode-common-hook)

(leaf abbrev
  :blackout abbrev-mode
  :custom
  (save-abbrevs . t)
  :config
  (abbrev-mode t))

(leaf iedit
  :doc "Edit multiple regions in the same way simultaneously."
  :tag "refactoring" "simultaneous" "region" "occurrence"
  :url "https://github.com/victorhge/iedit"
  :added "2022-10-31"
  :ensure t
  :bind ("C-c ;" . iedit-mode))

;; (leaf volatile-highlights
;;   :doc "Minor mode for visual feedback on some operations."
;;   :tag "wp" "convenience" "emulations"
;;   :url "http://www.emacswiki.org/emacs/download/volatile-highlights.el"
;;   :added "2022-11-01"
;;   :ensure t
;;   :blackout t
;;   ;; :after undo-tree
;;   :custom (Vhl/highlight-zero-width-ranges . t)
;;   :config
;;   (volatile-highlights-mode t)
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree)
;;   )

(leaf ws-butler
  :blackout t
  :ensure t
  :hook ((c-mode-common-hook text-mode-hook fundamental-mode-hook) . ws-butler-mode))

(leaf bool-flip
  :ensure t
  :bind ("C-c C-b" . bool-flip-do-flip))

(message "**** Check clang executable location")
(when (not (executable-find "clang")) (message "***** clang executable not found"))

(leaf highlight-numbers
  :doc "Highlight numbers in source code"
  :req "emacs-24" "parent-mode-2.0"
  :tag "emacs>=24"
  :url "https://github.com/Fanael/highlight-numbers"
  :added "2022-10-31"
  :emacs>= 24
  :ensure t
  :hook prog-mode-hook)
(leaf highlight-defined
  :doc "Syntax highlighting of known Elisp symbols"
  :req "emacs-24"
  :tag "emacs>=24"
  :url "https://github.com/Fanael/highlight-defined"
  :added "2022-10-31"
  :emacs>= 24
  :ensure t
  :hook emacs-lisp-mode-hook)

(leaf highlight-operators
  :doc "a face for operators in programming modes"
  :added "2022-10-31"
  :ensure t
  :hook c-mode-common-hook)

(leaf highlight-escape-sequences
  :doc "Highlight escape sequences"
  :tag "convenience"
  :url "https://github.com/dgutov/highlight-escape-sequences"
  :added "2022-10-31"
  :ensure t
  :config (hes-mode))

(leaf clang-format
  :doc "Format code using clang-format."
  :req "cl-lib-0.3"
  :tag "c" "tools"
  :url "https://github.com/emacsmirror/clang-format"
  :added "2022-10-31"
  :ensure t
  :when (executable-find "clang-format")
  ;; :bind ((:c-mode-base-map
  ;;         ("C-M-'" . #'clang-format-region))
  ;;        (:c++-mode-map
  ;;         ("C-M-'" . #'clang-format-region))
  ;;        (:c-mode-map
  ;;         ("C-M-'" . #'clang-format-region)))
  :preface
  (defun kb/c++-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c++-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c++-ts-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c++-ts-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c-ts-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c-ts-mode-map "C-M-'" #'clang-format-region))
  :hook (c-mode-hook . kb/c-bind-clang-format )
  :hook (c++-mode-hook . kb/c++-bind-clang-format)
  :hook (c-ts-mode-hook . kb/c-ts-bind-clang-format)
  :hook (c++-ts-mode-hook . kb/c++-ts-bind-clang-format)
  )

(leaf clang-format+
  :disabled t
  :doc "Minor mode for automatic clang-format application"
  :req "emacs-25.1" "clang-format-20180406.1514"
  :tag "clang-format" "c++" "c" "emacs>=25.1"
  :url "https://github.com/SavchenkoValeriy/emacs-clang-format-plus"
  :added "2022-10-31"
  :emacs>= 25.1
  :after clang-format
  :ensure t
  :custom (clang-format+-context . 'modification)
  :hook ((c-mode-common-hook c-ts-base-mode-hook) . clang-format+-mode)
  )

(leaf hideshow
  :doc "minor mode cmds to selectively display code/comment blocks"
  :tag "builtin"
  :added "2025-07-04")

(leaf mason
  :doc "Install all the programming tools easy"
  :tag "out-of-MELPA"
  :added "2025-08-01"
  :after eglot
  :require 'mason
  :hook (eglot-managed-mode-hook . kb/setup-mason-paths)
  :config (message "**** Loading mason")
  (defun kb/setup-mason-paths (&rest _)
    (message "***** Setting up mason server paths")
    (mason-setup-paths))
  (advice-add 'eglot-ensure :before 'kb/setup-mason-paths)
  (advice-add 'eglot :before 'kb/setup-mason-paths)
  )

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :tag "builtin"
  :added "2025-07-02"
  :hook ((python-mode-hook ruby-mode-hook elixir-mode-hook) . eglot-ensure)
  :custom
  ((eglot-send-changes-idle-time . 0.1)
   (eglot-extend-to-xref . t)              ; activate Eglot in referenced non-project files
   )

  :config
  (message "**** Loading eglot")
  ;; (defun kb/eglot-ensure-with-servers-path ()
  ;;   "Initialize mason installes server paths to be used by eglot."
  ;;   (require 'mason)
  ;;   (mason-setup-paths)
  ;;   (eglot-ensure))
  :config
  (require 'mason)
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (when (executable-find "haskell-language-server-wrapper")
    (message "**** found haskell-language-server-wrapper")
    (add-to-list 'eglot-server-programs
                 '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
    )
  :hook (((c-mode-common-hook c-ts-base-mode-hook) . eglot-ensure))

  :config
  ;; Setup eldoc the way I like it for emacs
  (defun my/setup-eldoc-for-eglot ()
    "Make sure Eldoc will show us all of the feedback at point."
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose-eagerly))
  :hook (eglot-managed-mode-hook . my/setup-eldoc-for-eglot)
  ;; (remove-hook 'eglot-managed-mode-hook #'#'my/setup-eldoc-for-eglot)

  :config
  (setopt eglot-confirm-server-edits '((eglot-rename . nil)
				       (t . maybe-summary)))

  :bind ((:eglot-diagnostics-map
          ("M-RET" . eglot-code-actions))
         (:eglot-mode-map ("C-c e r" . 'eglot-rename)
                          ("C-c e o" . 'eglot-code-action-organize-imports)
                          ("C-c e h" . 'eldoc)
                          ("<f6>" . 'xref-find-definitions)
                          ("C-c e a" . 'eglot-code-actions)))
  ;; :init
  ;; (eval-after-load 'eglot
  ;;   (define-key global-map (kbd "C-c e") 'eglot-mode-map))
  (eglot-inlay-hints-mode)
  )

(leaf flycheck-eglot
  :when (package-installed-p 'flycheck)
  :doc "Flycheck support for eglot"
  :req "emacs-28.1" "eglot-1.9" "flycheck-32"
  :tag "tools" "language" "convenience" "emacs>=28.1"
  :url "https://github.com/flycheck/flycheck-eglot"
  :added "2025-07-02"
  :emacs>= 28.1
  :ensure t
  :after eglot flycheck
  :config
  (global-flycheck-eglot-mode 1))

(leaf eglot-inactive-regions
  :disabled t
  :doc "Highlight inactive code regions with eglot power"
  :req "emacs-29.1"
  :tag "emacs>=29.1"
  :url "https://github.com/fargiolas/eglot-inactive-regions"
  :added "2025-07-07"
  :emacs>= 29.1
  :ensure t
  :after eglot
  :custom
  (eglot-inactive-regions-style . 'darken-foreground)
  (eglot-inactive-regions-opacity . 0.4)
  :config
  (defun kb/eglot-inactive-regions-hook ()
    (message "***** enable eglot-inactive-regions %S" (eglot-managed-p))
    (eglot-inactive-regions-mode (eglot-managed-p)))
  ;; (remove-hook 'eglot-managed-mode-hook #'kb/eglot-inactive-regions-hook)
  :hook (eglot-managed-mode-hook . kb/eglot-inactive-regions-hook)
  )


(leaf symbols-outline
  :doc "Display symbols (functions, variables, etc) in outline view"
  :req "emacs-27.1"
  :tag "outlines" "emacs>=27.1"
  :url "https://github.com/liushihao456/symbols-outline.el"
  :added "2025-07-11"
  :emacs>= 27.1
  :ensure t
  :bind ("C-c i" . #'symbols-outline-show)
  :custom ((symbols-outline-fetch-fn . #'symbols-outline-lsp-fetch)
           (symbols-outline-window-position . 'left))
  :config
  (symbols-outline-follow-mode))

(leaf eglot-codelens
  :disabled t
  :ensure nil
  :vc (:url "https://github.com/Gavinok/eglot-codelens.git" :vc-backend 'git :latest-release)
  :after eglot
  :require t
  :config
  (defun kb/codelens-mode-hook ()
    (message "***** enable eglot-codelens %S" (eglot-managed-p))
    (eglot-codelens-mode (eglot-managed-p)))
  ;; (remove-hook 'eglot-managed-mode-hook #'kb/codelens-mode-hook)
  :hook (eglot-managed-mode-hook . kb/codelens-mode-hook))

(leaf sideline-eglot
  :doc "Show eglot information with sideline"
  :req "emacs-29.1" "eglot-1.12.29" "sideline-0.1.0" "ht-2.4"
  :tag "eglot" "convenience" "emacs>=29.1"
  :url "https://github.com/emacs-sideline/sideline-eglot"
  :added "2025-07-02"
  :emacs>= 29.1
  :ensure t
  :after eglot sideline
  (add-to-list 'sideline-backends-right 'sideline-lsp))

(leaf eglot-signature-eldoc-talkative
  :doc "Make Eglot make ElDoc echo docs"
  :req "emacs-29.1" "eglot-1.16" "eldoc-1.14.0" "jsonrpc-1.0.23"
  :tag "lsp" "languages" "eldoc" "eglot" "documentation" "convenience" "emacs>=29.1"
  :url "https://codeberg.org/mekeor/eglot-signature-eldoc-talkative"
  :added "2025-07-02"
  :emacs>= 29.1
  :ensure t
  :after eglot eldoc jsonrpc)

(leaf breadcrumb
  :doc "Project and imenu-based breadcrumb paths"
  :req "emacs-28.1" "project-0.9.8"
  :tag "emacs>=28.1"
  :url "https://github.com/joaotavora/breadcrumb"
  :added "2025-07-02"
  :emacs>= 28.1
  :ensure t
  :config
  (breadcrumb-mode))

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :added "2025-07-04"
  :blackout t
  ;; :init
  ;; (setopt eldoc-echo-area-display-truncation-message t)
  ;; (setopt eldoc-echo-area-use-multiline-p nil)
  ;; Make sure Eldoc will show us all of the feedback at point.
  ;; no more clobbering
  ;; (global-eldoc-mode t)
  :custom
  ((eldoc-documentation-strategy . #'eldoc-documentation-compose-eagerly))
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  :config
   (eldoc-add-command-completions "paredit-")
   (eldoc-add-command-completions "combobulate-"))

(leaf eldoc-box
  :doc "Display documentation in childframe"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/casouri/eldoc-box"
  :added "2025-07-05"
  :emacs>= 27.1
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode)
  :config
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(leaf sideline
  :doc "Show information on the side"
  :req "emacs-28.1" "ht-2.4"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline"
  :added "2025-05-15"
  :emacs>= 28.1
  :blackout t
  :ensure t
  :hook ((flycheck-mode-hook . sideline-mode)
         (eglot-managed-mode-hook . sideline-mode)))

(leaf sideline-flycheck
  :when (package-installed-p 'flycheck)
  :doc "Show flycheck errors with sideline"
  :req "emacs-28.1" "sideline-0.1.1" "flycheck-0.14" "ht-2.4"
  :tag "flycheck" "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline-flycheck"
  :added "2025-05-15"
  :emacs>= 28.1
  :ensure t
  :after sideline flycheck
  :hook (flycheck-mode-hook . sideline-flycheck-setup)
  :config
  (add-to-list 'sideline-backends-left 'sideline-flycheck))

(leaf cmake-mode
  :disabled t
  :doc "Major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2025-01-17"
  :emacs>= 24.1
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init
  (defun kb/cmake-mode-setup ()
      (message "***** cmakemode custom")
      (setq fill-column 80)
      (auto-fill-mode)
      (setq cmake-tab-width 4)
      (setq indent-tabs-mode nil)
      ;; NOTE: default eglot-server-programs contains following setting for cmake-mode and cmake-ts-mode.
      ;; ,(eglot-alternatives '((("neocmakelsp" "--stdio") "cmake-language-server")))
      ;; This is causing problems as ("neocmakelsp" "--stdio") is improperly passed to (eglot--find-executable)
      ;; To override this set it explicit
      (add-to-list 'eglot-server-programs
                   '(cmake-mode . ("neocmakelsp" "--stdio")))
      (eglot-ensure))
  :hook (cmake-mode-hook . kb/cmake-mode-setup))

(leaf cmake-ts-mode
  :doc "tree-sitter support for CMake"
  :tag "builtin"
  :added "2025-08-20"
  (defun kb/cmake-mode-setup ()
      (message "***** cmake-ts-mode custom")
      (setq fill-column 80)
      ;; (auto-fill-mode)
      ;; (setq cmake-tab-width 4)
      ;; (setq indent-tabs-mode nil)
      ;; NOTE: default eglot-server-programs contains following setting for cmake-mode and cmake-ts-mode.
      ;; ,(eglot-alternatives '((("neocmakelsp" "--stdio") "cmake-language-server")))
      ;; This is causing problems as ("neocmakelsp" "--stdio") is improperly passed to (eglot--find-executable)
      ;; To override this set it explicit
      (add-to-list 'eglot-server-programs
                   '(cmake-mode . ("neocmakelsp" "--stdio")))
      (eglot-ensure))
  :hook (cmake-ts-mode-hook . kb/cmake-mode-setup))

(leaf cmake-font-lock
  :doc "Advanced, type aware, highlight support for CMake"
  :req "cmake-mode-0.0"
  :tag "languages" "faces"
  :url "https://github.com/Lindydancer/cmake-font-lock"
  :added "2025-01-17"
  :ensure t
  :after cmake-mode)

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :added "2025-07-11"
  :ensure t
  :after avy)

(leaf treemacs
  :doc "A tree style file explorer package."
  :req "emacs-26.1" "cl-lib-0.5" "dash-2.11.0" "s-1.12.0" "ace-window-0.9.0" "pfuture-1.7" "hydra-0.13.2" "ht-2.2" "cfrs-1.3.2"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2025-01-17"
  :emacs>= 26.1
  :ensure t
  :after ace-window pfuture hydra cfrs
  :bind ("<f8>" . treemacs)
  :custom-face
  ;; update font size
  (treemacs-directory-face . '((t (:inherit font-lock-function-name-face :height 0.8))))
  (treemacs-file-face . '((t (:inherit default :height 0.9))))
  (treemacs-root-face ' '((t (:inherit font-lock-constant :height 1.1))))
  (treemacs-root-unreadable-face . '((t (:inherit treemacs-root-face :height 0.9))))
  (treemacs-root-remote-face . '((t (:inherit font-lock-function-name-face :height 2.0))))
  (treemacs-git-modified-face . '((t (:inherit font-lock-variable-name-face :height 0.9))))
  (treemacs-git-ignored-face . '((t (:inherit font-lock-comment-face :height 0.9))))
  (treemacs-git-untracked-face . '((t (:inherit font-lock-string-face :height 0.9))))
  :custom
  (treemacs-space-between-root-nodes . nil)
  )

(leaf separedit
  :doc "Edit comment/string/docstring/code block in separate buffer"
  :req "emacs-25.1" "dash-2.18" "edit-indirect-0.1.11"
  :tag "docs" "languages" "tools" "emacs>=25.1"
  :url "https://github.com/twlz0ne/separedit.el"
  :added "2025-05-07"
  :emacs>= 25.1
  :ensure t
  :custom
  (separedit-default-mode . 'markdown-mode)
  (separedit-preserve-string-indentation . t)
  (separedit-continue-fill-column . t)
  (separedit-write-file-when-execute-save . t)
  (separedit-remove-trailing-spaces-in-comment . t)
  :init
  (defun kb/bind-separedit () (keymap-set prog-mode-map "C-c '" #'separedit))
  :hook (prog-mode-hook . kb/bind-separedit)
  :bind
  ((:prog-mode-map
    ((kbd "C-c '") . #'separedit))
   (:minibuffer-local-map
    ((kbd "C-c '") . #'separedit))
   (:help-mode-map
    ((kbd "C-c '") . #'separedit))
   ;; (:helpful-mode-map
   ;;  ((kbd "C-c '") . #'separedit))
   )
  ;; :init
  ;; (require 'separedit)
  ;; (define-key prog-mode-map "C-c '" 'separedit)
  ;; (define-key c-mode-map "C-c '" 'separedit)
  ;; (define-key c++-mode-map "C-c '" 'separedit)
  )

(leaf dirvish
  :doc "A modern file manager based on dired mode"
  :req "emacs-27.1" "transient-0.3.7"
  :tag "convenience" "files" "emacs>=27.1"
  :url "https://github.com/alexluigit/dirvish"
  :added "2025-02-04"
  :emacs>= 27.1
  :ensure t
  :custom (
           ;; subtree-state: A indicator for directory expanding state.
           ;; all-the-icons: File icons provided by all-the-icons.el.
           ;; vscode-icon: File icons provided by vscode-icon.el.
           ;; collapse: Collapse unique nested paths.
           ;; git-msg: Append git commit message to filename.
           ;; vc-state: The version control state at left fringe.
           ;; file-size: Show file size or directories file count at right fringe.
           ;; file-time (newly added): Show file modification time before the file-size
           ;; (dirvish-attributes . '(vc-state file-time)) ; file-time vc-state vscode-icon
           (dirvish-attributes . '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
           ;; Placement
           ;; (setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
           ;; (setq dirvish-use-mode-line nil)       ; hide mode line
           (dirvish-use-header-line . 'global)    ; make header line span all panes

           ;; Height
   ;;; '(25 . 35) means
   ;;; - height in single window sessions is 25
   ;;; - height in full-frame sessions is 35
           (dirvish-header-line-height . '(25 . 35))
           (dirvish-mode-line-height . 25) ; shorthand for '(25 . 25)

           ;; Segments
   ;;; 1. the order of segments *matters* here
   ;;; 2. it's ok to place raw string inside
           (dirvish-header-line-format . '(:left (path) :right (free-space)))
           (dirvish-mode-line-format . '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

           (dirvish-quick-access-entries . ; It's a custom option, `setq' won't work
                                         '(("h" "~/"             "Home")
                                           ("d" "~/Downloads/"   "Downloads")
                                           ("m" "/mnt/"          "Drives")
                                           ("p" "~/projects/"    "Projects")))
           (dired-listing-switches . "-l --almost-all --human-readable --group-directories-first --no-group")

           (dired-mouse-drag-files . t)                   ; added in Emacs 29
           (mouse-drag-and-drop-region-cross-program . t) ; added in Emacs 29
           (mouse-1-click-follows-link . nil)
           )
  :bind
  (
   ;; ("C-c f" . dirvish-fd)
   (:dirvish-mode-map ("a" . dirvish-quick-access)
                      ("f" . dirvish-file-info-menu)
                      ("y" . dirvish-yank-menu)
                      ("N" . dirvish-narrow)
                      ("^"   . dirvish-history-last)
                      ("h"   . dirvish-history-jump) ; remapped `describe-mode'
                      ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
                      ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
                      ("TAB" . dirvish-subtree-toggle)
                      ("M-f" . dirvish-history-go-forward)
                      ("M-b" . dirvish-history-go-backward)
                      ("M-l" . dirvish-ls-switches-menu)
                      ("M-m" . dirvish-mark-menu)
                      ("M-t" . dirvish-layout-toggle)
                      ("M-s" . dirvish-setup-menu)
                      ("M-e" . dirvish-emerge-menu)
                      ("M-j" . dirvish-fd-jump)
                      ((kbd "<mouse-1>") . 'dirvish-subtree-toggle-or-open)
                      ((kbd "<mouse-2>") . 'dired-mouse-find-file-other-window)
                      ((kbd "<mouse-3>") . 'dired-mouse-find-file)))
  :init
  (dirvish-override-dired-mode)
  :config
  (setopt dirvish-fd-program (executable-find "fdfind"))
  (setopt dirvish-preview-dispatchers (remove 'epub dirvish-preview-dispatchers))
  (setopt dirvish-preview-dispatchers (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))
  (when (executable-find "exa")
    (message "found exa")
    (dirvish-define-preview exa (file)
      "Use `exa' to generate directory preview."
      :require ("exa") ; tell Dirvish to check if we have the executable
      (when (file-directory-p file) ; we only interest in directories here
        `(shell . ("exa" "-al" "--color=always" "--icons"
                   "--group-directories-first" ,file))))
    (setopt dirvish-preview-dispatchers (add-to-list 'dirvish-preview-dispatchers 'exa))
    )
  )

(leaf adaptive-wrap
  :ensure t
  :hook ((text-mode-hook . adaptive-wrap-prefix-mode)))

(leaf which-key
  :doc "Display available keybindings in popup"
  :tag "builtin"
  :added "2025-01-16"
  :blackout t
  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  :custom ((which-key-idle-delay . 1e-100)
           (which-key-idle-secondary-delay . 1e-100)
           (which-key-max-display-columns . nil)
           (which-key-side-window-slot . -10)
           (which-key-show-docstrings . t)
           (which-key-show-early-on-C-h . nil)
           (which-key-sort-uppercase-first . nil)
           (which-key-min-display-lines . 6)
           (which-key-separator . " → ")
           (which-key-sort-order . #'which-key-key-order-alpha)
           (which-key-add-column-padding . 1))
  :config
  (message "**** Configure which-key")
  (defun kb/add-hydra-which-key-replacement ()
    (add-to-list 'which-key-replacement-alist
                 '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1"))))
  ;; :hook ((emacs-startup-hook . which-key-mode)
  ;;        (which-key-mode-hook . kb/add-hydra-which-key-replacement))
)

(leaf hercules
  :doc "An auto-magical, which-key-based hydra banisher."
  :req "emacs-24.4" "which-key-3.3.2"
  :tag "convenience" "emacs>=24.4"
  :url "https://gitlab.com/jjzmajic/hercules"
  :added "2025-01-16"
  :emacs>= 24.4
  :ensure t
  :after which-key)

(leaf which-key-posframe
  :doc "Using posframe to show which-key"
  :req "emacs-26.0" "posframe-1.4.0" "which-key-3.6.0"
  :tag "tooltip" "bindings" "convenience" "emacs>=26.0"
  :url "https://github.com/emacsorphanage/which-key-posframe"
  :added "2025-01-16"
  :emacs>= 26.0
  :ensure t
  :after which-key
  :custom ((which-key-posframe-font . "Liberation Mono")
           ;; (which-key-posframe-poshandler . #'posframe-poshandler-point-1)
           (which-key-posframe-poshandler . #' posframe-poshandler-frame-top-center)
           )
  :config
  (which-key-posframe-mode))

(leaf ninja-mode
  :doc "Major mode for editing .ninja files."
  :req "emacs-24.3"
  :tag "languages" "emacs>=24.3"
  :url "https://github.com/jhasse/ninja-emacs"
  :added "2025-01-16"
  :emacs>= 24.3
  :ensure t)
(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :url "https://github.com/yoshiki/yaml-mode"
  :added "2022-10-31"
  :emacs>= 24.1
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-mode-hook yaml-ts-mode-hook) . eglot-ensure)
  :custom-face (font-lock-variable-name-face . '((t (:foreground "#cba6f7"))))
  :config (message "**** Loading yaml-mode")
  )
;; (leaf yaml-imenu
;;   :doc "Enhancement of the imenu support in yaml-mode"
;;   :req "emacs-24.4" "yaml-mode-0"
;;   :tag "imenu" "convenience" "outlining" "emacs>=24.4"
;;   :url "https://github.com/knu/yaml-imenu.el"
;;   :added "2025-02-05"
;;   :emacs>= 24.4
;;   :ensure t
;;   :after yaml-mode
;;   :config
;;   (yaml-imenu-enable))
(leaf yaml
  :doc "YAML parser for Elisp"
  :req "emacs-25.1"
  :tag "tools" "emacs>=25.1"
  :url "https://github.com/zkry/yaml.el"
  :added "2025-02-05"
  :emacs>= 25.1
  :ensure t)

(leaf flymake-yaml
  :doc "A flymake handler for YAML"
  :req "flymake-easy-0.1"
  :tag "yaml"
  :url "https://github.com/yasuyk/flymake-yaml"
  :added "2025-07-11"
  :ensure t
  ;; :hook (yaml-mode-hook . #'flymake-yaml-load)
)

(unless (executable-find "yamllint") (message "**** ! no yamllint executable found"))
(leaf flymake-yamllint
  :doc "YAML linter with yamllint"
  :when (executable-find "yamllint")
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/shaohme/flymake-yamllint"
  :added "2025-07-11"
  :emacs>= 26.1
  :ensure t)

(leaf ietf-docs
  :doc "Fetch, Cache and Load IETF documents"
  :tag "rfc" "ietf"
  :url "https://github.com/choppsv1/ietf-docs"
  :added "2022-10-31"
  :ensure t
  ;; :bind (("C-c k o" . ietf-docs-open-at-point))
  )
(leaf rfc-mode
  :doc "RFC document browser and viewer"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/galdor/rfc-mode"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t)

(leaf protobuf-mode
  :disabled t
  :doc "major mode for editing protocol buffers."
  :tag "languages" "protobuf" "google"
  :added "2022-10-31"
  :config
  (defconst kb/protobuf-style
    '("linux"
      (c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode-hook . (lambda () (c-add-style "kb/protobuf" kb/protobuf-style t)))
  )
(leaf treemacs-icons-dired
  :disabled t
  :doc "Treemacs icons for dired"
  :req "treemacs-0.0" "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :after treemacs
  :hook (dired-mode-hook . treemacs-icons-dired-enable-once))

(leaf hydra
  :disabled t
  :doc "Make bindings that stick around."
  :req "cl-lib-0.5" "lv-0"
  :tag "bindings"
  :url "https://github.com/abo-abo/hydra"
  :added "2022-10-31"
  :after lv
  :config
  (defhydra hydra-goto (global-map "M-g")
    "goto"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit"))

  (defhydra hydra-switch-buffers
    (global-map "C-c s"
                :timeout 4
                :hint "Switch Buffers and windows")
    "Buffers"
    ("n" next-multiframe-window "Next window" :group "HSB" :which-key "Next Window")
    ("p" previous-multiframe-window "Previous window" :group "HSB" :which-key "Prev Window")
    ("j" switch-to-next-buffer "Next buffer" :group "HSB" :which-key "Next Buffer")
    ("k" switch-to-prev-buffer "Previous buffer" :group "HSB" :which-key "Prev Buffer")
    ("q" nil "done" :exit t :group "HSB" :which-key "Quit"))
  )

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "files" "matching" "emacs>=28.1"
  :url "https://github.com/minad/consult"
  :added "2025-06-06"
  :emacs>= 28.1
  :ensure t
  :after compat
  ;; Replace bindings. Lazily loaded by `use-package'.

  ;;        ("C-c j" . counsel-git-grep)
  ;;        ("C-c L" . counsel-git-log)
  ;;        ("C-c J" . counsel-file-jump)
  ;;        ("C-x l" . counsel-locate)
  ;;        ;; ("C-c t" . counsel-load-theme)
  ;;        ("C-c C-o" . counsel-imenu)
  ;;        ([remap insert-register] . counsel-register)
  ;;        (:minibuffer-local-map
  ;;         ("C-r" . counsel-minibuffer-history));

  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c t" . consult-theme)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x p p" . consult-projectile)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ([remap copy-to-register] . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ([remap insert-register] . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ;; ("C-c j" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (:isearch-mode-map
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
          )
         (
          ;; Minibuffer history
          :minibuffer-local-map
          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
          ("M-r" . consult-history)))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))

  :custom
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key . "C-+") ;; "<"

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

(leaf consult-eglot
  :doc "A consulting-read interface for eglot"
  :req "emacs-27.1" "eglot-1.7" "consult-0.31" "project-0.3.0"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/mohkale/consult-eglot"
  :added "2025-07-02"
  :emacs>= 27.1
  :ensure t
  :after eglot consult
  :bind ("M-s s" . #'consult-eglot-symbols))

(leaf consult-eglot-embark
  :doc "Embark integration for `consult-eglot'"
  :req "emacs-27.1" "consult-eglot-0.3" "embark-consult-1.0"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/mohkale/consult-eglot"
  :added "2025-07-02"
  :emacs>= 27.1
  :ensure t
  :after consult-eglot embark-consult
  :config
  (consult-eglot-embark-mode))

(leaf consult-projectile
  :doc "Consult integration for projectile"
  :req "emacs-25.1" "consult-0.12" "projectile-2.5.0"
  :tag "convenience" "emacs>=25.1"
  :url "https://gitlab.com/OlMon/consult-projectile"
  :added "2025-06-06"
  :emacs>= 25.1
  :ensure t
  :require t
  :after projectile
  :hook (projectile-mode-hook . (lambda ()
                                  (when projectile-mode
                                    (define-key projectile-mode-map [remap projectile-switch-project] #'consult-projectile)
                                    (define-key projectile-mode-map [remap projectile-recentf] #'consult-projectile-recentf)
                                    (define-key projectile-mode-map [remap projectile-find-file] #'consult-projectile-find-file)
                                    (define-key projectile-mode-map [remap projectile-find-dir] #'consult-projectile-find-dir)
                                    (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'consult-projectile-switch-to-buffer))))
  :config
  (setopt consult-projectile-sources
          (append '(consult-projectile--source-projectile-dir consult-projectile--source-projectile-recentf)
                  consult-projectile-sources)))

(leaf consult-flycheck
  :when (package-installed-p 'flycheck)
  :doc "Provides the command `consult-flycheck'"
  :req "emacs-28.1" "consult-1.8" "flycheck-34"
  :tag "completion" "tools" "languages" "emacs>=28.1"
  :url "https://github.com/minad/consult-flycheck"
  :added "2025-06-06"
  :emacs>= 28.1
  :ensure t
  :after consult flycheck)

(leaf consult-dir
  :doc "Insert paths into the minibuffer prompt"
  :req "emacs-27.1" "consult-1.0"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/karthink/consult-dir"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after consult
  :bind (("C-x C-d" . #'consult-dir)
         (:minibuffer-local-completion-map ("C-x C-d" . #'consult-dir)
                                           ;; ("C-x C-j" . #'consult-dir-jump-file) ;; fails with (void-function consult--async-split-style)
                                           )
         )
  :hook
  ((projectile-mode-hook . (lambda (&rest _) (setq consult-dir-project-list-function 'consult-dir-projectile-dirs)))))

(leaf consult-ls-git
  :doc "Consult integration for git"
  :req "emacs-27.1" "consult-0.16"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/rcj/consult-ls-git"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after consult
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(unless (executable-find "gh") (message "**** Github cli command `gh` not found"))
(leaf consult-gh
  :doc "Consulting GitHub Client"
  :req "emacs-29.4" "consult-2.0" "markdown-mode-2.6" "ox-gfm-1.0"
  :tag "vc" "tools" "matching" "convenience" "emacs>=29.4"
  :url "https://github.com/armindarvish/consult-gh"
  :added "2025-07-15"
  :emacs>= 29.4
  :ensure t
  :when (executable-find "gh")
  :after consult markdown-mode ox-gfm
  :blackout t
  :custom ((consult-gh-default-clone-directory . "~/projects/")
           (consult-gh-show-preview . t)
           (consult-gh-preview-key . "C-o")
           (consult-gh-repo-action . #'consult-gh--repo-browse-files-action)
           (consult-gh-issue-action . #'consult-gh--issue-view-action)
           (consult-gh-pr-action . #'consult-gh--pr-view-action)
           (consult-gh-code-action . #'consult-gh--code-view-action)
           (consult-gh-file-action . #'consult-gh--files-view-action)
           (consult-gh-notifications-action . #'consult-gh--notifications-action)
           (consult-gh-dashboard-action . #'consult-gh--dashboard-action)
           (consult-gh-large-file-warning-threshold . 2500000)
           (consult-gh-prioritize-local-folder . 'suggest))
  :config
  ;; Remember visited orgs and repos across sessions
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))

(leaf consult-gh-embark
  :doc "Embark Actions for consult-gh"
  :req "emacs-29.4" "consult-2.0" "consult-gh-2.6" "embark-consult-1.1"
  :tag "completion" "forges" "repositories" "git" "matching" "emacs>=29.4"
  :url "https://github.com/armindarvish/consult-gh"
  :added "2025-07-15"
  :emacs>= 29.4
  :ensure t
  :after consult consult-gh embark-consult
  :blackout t
  :config
  (consult-gh-embark-mode +1))

(leaf consult-gh-forge
  :doc "Magit/Forge Integration for consult-gh"
  :req "emacs-29.4" "consult-2.0" "forge-0.3.3" "consult-gh-2.6"
  :tag "completion" "forges" "repositories" "git" "matching" "emacs>=29.4"
  :url "https://github.com/armindarvish/consult-gh"
  :added "2025-07-15"
  :emacs>= 29.4
  :ensure t
  :after consult forge consult-gh
  :blackout t
  :custom (consult-gh-forge-timeout-seconds . 20)
  :config (consult-gh-forge-mode +1))

(leaf consult-flyspell
  :doc "Consult integration for flyspell"
  :req "emacs-25.1" "consult-0.12"
  :tag "convenience" "emacs>=25.1"
  :url "https://gitlab.com/OlMon/consult-flyspell"
  :added "2025-07-15"
  :emacs>= 25.1
  :ensure t
  :after consult
  ;; :config
  ;; default settings
  ;; (setq consult-flyspell-select-function nil
  ;;       consult-flyspell-set-point-after-word t
  ;;       consult-flyspell-always-check-buffer nil)
  )

(leaf vertico
  :doc "VERTical Interactive COmpletion."
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "files" "convenience" "emacs>=28.1"
  :url "https://github.com/minad/vertico"
  :added "2025-06-10"
  :emacs>= 28.1
  :ensure t
  :bind (:vertico-map
         ;; Option 1: Additional bindings
         ("?"        . #'minibuffer-completion-help)
         ("M-RET"    . #'minibuffer-force-complete-and-exit)
         ("M-TAB"    . #'minibuffer-complete)
         ("\177"     . vertico-directory-delete-char)
         ("<return>" . vertico-directory-enter)
         ;; ("/"        . jcs-vertico-/)
         ;; (":"        . jcs-vertico-:)

         ;; Option 2: Replace `vertico-insert' to enable TAB prefix expansion.
         ;; (keymap-set vertico-map "TAB" #'minibuffer-complete)
         )
  :hook emacs-startup-hook
  :custom ((vertico-cycle . t)
           (vertico-resize . t)
           (vertico-scroll-margin . 0)
           (vertico-preselect . 'first))


  ;; do not consider case significant in completion (GNU Emacs default)
  ;; (setq completion-ignore-case t
  ;;       read-file-name-completion-ignore-case t
  ;;       read-buffer-completion-ignore-case t)
  :config
  (message "**** Configure vertico")
  (vertico-mode)
  (vertico-mouse-mode +1)
  ;; (setq completion-in-region-function
  ;;       (lambda (&rest args)
  ;;         (apply (if vertico-mode
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region)
  ;;                args)))

  ;; Don't re-sort buffer candidates. The recency order is correct.
  ;; (vertico-multiform-mode +1)
  ;; (setq vertico-multiform-categories
  ;;       '((buffer (vertico-sort-function . copy-sequence))))
  )

;; Configure directory extension.
(leaf vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(leaf vertico-posframe
  :doc "Using posframe to show Vertico"
  :req "emacs-26.0" "posframe-1.4.0" "vertico-1.10"
  :tag "vertico" "matching" "convenience" "abbrev" "emacs>=26.0"
  :url "https://github.com/tumashu/vertico-posframe"
  :added "2025-06-06"
  :emacs>= 26.0
  :ensure t
  :after vertico
  :hook (vertico-mode-hook . vertico-posframe-mode)
  :config
  (message "**** Configure vertico-posframe")
  (setq vertico-posframe-poshandler #'posframe-poshandler-point-top-left-corner)
  (setq vertico-multiform-commands
        '((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-bottom-center)
           (vertico-posframe-border-width . 5)
           ;; NOTE: This is useful when emacs is used in both in X and
           ;; terminal, for posframe do not work well in terminal, so
           ;; vertico-buffer-mode will be used as fallback at the
           ;; moment.
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-imenu
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-left-or-right-other-corner)
           (vertico-posframe-border-width . 3)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-buffer
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-window-top-center)
           (vertico-posframe-border-width . 1)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-ripgrep
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-window-top-center)
           (vertico-posframe-border-width . 3)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-goto-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-window-top-center)
           (vertico-posframe-border-width . 3)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (execute-extended-command
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-bottom-center)
           (vertico-posframe-border-width . 7)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (lsp-find-references
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-right-corner)
           (vertico-posframe-border-width . 4)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (embark-prefix-help-command
           posframe grid
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 4)
           (vertico-posframe-fallback-mode . vertico-grid-mode))
          (casual-ibuffer-tmenu
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 4)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (setq vertico-posframe-parameters
        '((left-fringe . 3)
          (right-fringe . 3)))
  )

(leaf vertico-multiform
  :hook (vertico-mode-hook . vertico-multiform-mode)
  :config
  (add-to-list 'vertico-multiform-categories
               '(file (vertico-sort-function . vertico-flx-sort-files)))
  (add-to-list 'vertico-multiform-categories
               '(embark-keybinding grid))
  )

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/minad/marginalia"
  :added "2022-10-31"
  :emacs>= 27.1
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         (:minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  ;; The :init configuration is always executed (Not lazy!)
  :custom (marginalia-align . 'right)
  :hook emacs-startup-hook)

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-28.1" "compat-30"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-06-06"
  :emacs>= 28.1
  :ensure t
  :commands (embark-info-lookup-symbol embark-save-unicode-character)
  :after compat
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("M-." . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
         ("<f2> i" . #'embark-info-lookup-symbol)
         ("<f2> u" . #'embark-save-unicode-character)
         )
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command . #'embark-prefix-help-command)
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  (embark-prompter . 'embark-completing-read-prompter)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (message "Configure embark")
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (keymap-set minibuffer-local-map "M-." 'kb/embark-preview)
  (defun kb/embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command

taken from: https://github.com/Gavinok/emacs.d.git"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))
  )


(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-28.1" "compat-30" "embark-1.1" "consult-1.8"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-06-06"
  :emacs>= 28.1
  :after embark consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))
(leaf embark-vc
  :doc "Embark actions for various version control integrations"
  :req "emacs-27.1" "embark-0.21.1" "forge-0.3" "compat-29.1.3.0"
  :tag "vc" "unix" "tools" "terminals" "matching" "convenience" "emacs>=27.1"
  :url "https://github.com/elken/embark-vc"
  :added "2025-06-22"
  :emacs>= 27.1
  :after embark
  :ensure t
  :require t)

;; (leaf completion-preview
;;   :doc "Preview completion with inline overlay"
;;   :tag "builtin"
;;   :added "2025-06-12"
;;   :config
;;   (global-completion-preview-mode))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :added "2025-06-06"
  :emacs>= 25.1
  :ensure t
  :custom
  ((completion-preview-sort-function . #'prescient-completion-sort)
   ;; The default settings seem a little forgetful to me. Let's try
   ;; this out.
   (prescient-history-length . 1000)
   (prescient-sort-full-matches-first . t))
  :after prescient
  :config
  (prescient-persist-mode))

(leaf vertico-prescient
  :ensure t
  :after vertico
  :config
  (vertico-prescient-mode))
(leaf corfu-prescient
  :ensure t
  :after corfu
  :config
  (corfu-prescient-mode))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :req "emacs-28.1" "compat-30"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/corfu"
  :added "2025-06-06"
  :emacs>= 28.1
  :ensure t
  :after compat
  ;; Optional customizations
  :custom
  (corfu-cycle . t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary . nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match . nil)      ;; Never quit, even if there is no match
  (corfu-preview-current . nil)    ;; Disable current candidate preview
  (corfu-preselect . 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match . nil)     ;; Configure handling of exact matches
  (corfu-auto . t)
  (corfu-auto-prefix . 2)
  (corfu-auto-delay . 0.1)
  (corfu-echo-delay . 0.25)

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :hook (eshell-mode-hook . (lambda () (setq-local corfu-quit-at-boundary t
                                              corfu-quit-no-match t
                                              corfu-auto nil)
                              (corfu-mode)))
  :bind ((:corfu-map
         ("M-SPC" . corfu-insert-separator)
         ("RET" . nil) ; Leave my enter alone!
         ("TAB" . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("M-RET" . corfu-insert)
         ("S-RET" . corfu-insert)
         ("C-RET" . corfu-insert)))
  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-echo-mode -1))

(leaf corfu-popupinfo
  :after corfu
  :hook (corfu-mode-hook . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay . '(0.25 . 0.1))
  (corfu-popupinfo-hide . nil)
  :config
  (corfu-popupinfo-mode)
  )

(leaf corfu-terminal
  :doc "Corfu popup on terminal"
  :req "emacs-26.1" "corfu-0.36" "popon-0.13"
  :tag "convenience" "emacs>=26.1"
  :url "https://codeberg.org/akib/emacs-corfu-terminal"
  :added "2025-06-26"
  :emacs>= 26.1
  :unless (display-graphic-p)
  :ensure t
  :after corfu popon
  :config
  (corfu-terminal-mode))

(leaf envrc
  :doc "Support for `direnv' that operates buffer-locally"
  :req "emacs-27.1" "inheritenv-0.1" "seq-2.24"
  :tag "tools" "processes" "emacs>=27.1"
  :url "https://github.com/purcell/envrc"
  :added "2025-06-12"
  :emacs>= 27.1
  :ensure t
  :after inheritenv
  :config
  (envrc-global-mode))

(leaf cape
  :doc "Completion At Point Extensions"
  :req "emacs-28.1" "compat-30"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/cape"
  :added "2025-06-11"
  :emacs>= 28.1
  :ensure t
  :after compat
  ;; :init
  ;; ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (defun kb/add-shell-completion ()
  ;;   (interactive)
  ;;   (add-hook 'completion-at-point-functions 'cape-history)
  ;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point))
  ;; (add-hook 'shell-mode-hook #'kb/add-shell-completion nil t)
  :bind (("C-c a" . cape-prefix-map)
         ("C-c f" . cape-file))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)

  :config
  ;; Make capfs composable
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)

  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev 50 t)
  ;; (add-hook 'completion-at-point-functions #'cape-file 50 t)
  ;; (add-hook 'completion-at-point-functions #'cape-emoji 50 t)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)

  ;; (defun my-cape-dabbrev-accept-all ()
  ;;   (cape-wrap-accept-all #'cape-dabbrev))
  ;; (add-hook 'completion-at-point-functions #'my-cape-dabbrev-accept-all)

  ;; (add-to-list 'completion-at-point-functions #'cape-file t)
  ;; ;; Nice completion to have available everywhere
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  ;; (add-to-list 'completion-at-point-functions #'cape-emoji t)

  ;; Example 6: Define interactive Capf which can be bound to a key.  Here we wrap
  ;; the `elisp-completion-at-point' such that we can complete Elisp code
  ;; explicitly in arbitrary buffers.
  (keymap-global-set "C-c a e" (cape-capf-interactive #'elisp-completion-at-point))

  ;; (defun ignore-elisp-keywords (sym)
  ;;   (not (keywordp sym)))
  ;; (setq-local completion-at-point-functions
  ;;             (list (cape-capf-predicate #'elisp-completion-at-point
  ;;                                        #'ignore-elisp-keywords)))
  (when (< emacs-major-version 29)
    ;; Silence then pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; Ensure that pcomplete does not write to the buffer and behaves as
    ;; pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  ;; (defalias 'cape-dabbrev-min-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  ;; (setq-local completion-at-point-functions (list (cape-capf-prefix-length #'cape-dabbrev 2)))
  )

(leaf tempel
  :doc "Tempo templates/snippets with in-buffer field editing"
  :req "emacs-28.1" "compat-30"
  :tag "text" "tools" "languages" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/tempel"
  :added "2025-07-04"
  :emacs>= 28.1
  :ensure t
  :after compat
  :hook ((prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf))
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         (:tempel-map
          ([remap keyboard-escape-quit] . tempel-done)
          ("TAB" . tempel-next)
          ("<backtab>" . tempel-previous)))
  :config
  (defun tempel-include (elt)
    "Support i as a way to import another template"
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'tempel-include)

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))

(leaf eglot-tempel
  :doc "Use tempel to expand snippets from eglot"
  :req "eglot-1.9" "tempel-0.5" "emacs-29.1" "peg-1.0.1"
  :tag "tools" "languages" "convenience" "emacs>=29.1"
  :url "https://github.com/fejfighter/eglot-tempel"
  :added "2025-07-04"
  :emacs>= 29.1
  :ensure t
  :after eglot tempel peg
  :config
  (eglot-tempel-mode t))

;; Use Dabbrev with Corfu!
(leaf dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(leaf savehist
  :config
  (savehist-mode))

(leaf helpful
  :doc "A better *help* buffer"
  :req "emacs-25" "dash-2.18.0" "s-1.11.0" "f-0.20.0" "elisp-refs-1.2"
  :tag "lisp" "help" "emacs>=25"
  :url "https://github.com/Wilfred/helpful"
  :added "2025-06-06"
  :emacs>= 25
  :ensure t
  :bind
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-command] . #'helpful-command)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-key] . #'helpful-key)
   ;; for this in lisp modes.
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function))
  )

(leaf fzf
  :doc "A front-end for fzf."
  :req "emacs-24.4"
  :tag "search" "fuzzy" "fzf" "emacs>=24.4"
  :url "https://github.com/bling/fzf.el"
  :added "2023-02-28"
  :emacs>= 24.4
  :ensure t)

(leaf smart-compile
  :doc "An interface to `compile'"
  :tag "unix" "tools"
  :url "https://github.com/zenitani/elisp"
  :added "2025-01-17"
  :ensure t
  :require smart-compile)

;; (leaf zygospore
;;   :doc "Reversible C-x 1 (delete-other-windows)"
;;   :url "https://github.com/louiskottmann/zygospore.el"
;;   :added "2025-01-17"
;;   :ensure t
;;   :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(leaf wgrep
  :doc "Writable grep buffer."
  :req "emacs-25.1"
  :tag "extensions" "edit" "grep" "emacs>=25.1"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el"
  :added "2025-06-22"
  :emacs>= 25.1
  :ensure t)

;; Use silversearcher-ag if available.
(unless (executable-find "ag") (warn "Command: silversearcher-ag not found"))
(leaf ag
  :doc "A front-end for ag ('the silver searcher'), the C ack replacement."
  :req "dash-2.8.0" "s-1.9.0" "cl-lib-0.5"
  :url "https://github.com/Wilfred/ag.el"
  :added "2022-10-31"
  :when (executable-find "ag")
  :ensure t
  :config
  (leaf wgrep-ag
    :doc "Writable ag buffer and apply the changes to files"
    :req "wgrep-2.3.2"
    :tag "extensions" "edit" "grep"
    :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el"
    :added "2022-10-31"
    :ensure t
    :after wgrep)
  )
(leaf consult-ag
  ;; fails with error:
  ;; consult--read: Symbol’s function definition is void: consult--async-command
  :disabled t
  :doc "The silver searcher integration using Consult"
  :req "emacs-27.1" "consult-0.32"
  :tag "emacs>=27.1"
  :when (executable-find "ag")
  :url "https://github.com/yadex205/consult-ag"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after consult)

;; Use ripgrep if available
(unless (executable-find "rg") (warn "Command: ripgrep not found"))
(leaf rg
  :doc "A search tool based on ripgrep"
  :tag "tools" "matching" "emacs>=25.1"
  :url "https://github.com/dajva/rg.el"
  :added "2022-10-31"
  :when (executable-find "rg")
  :emacs>= 25.1
  :ensure t
  :after wgrep)
(leaf ripgrep
  :doc "Front-end for ripgrep, a command line search tool"
  :tag "search" "grep" "sift" "ag" "pt" "ack" "ripgrep"
  :url "https://github.com/nlamirault/ripgrep.el"
  :added "2022-10-31"
  :when (executable-find "rg")
  :ensure t
  :config
  (leaf projectile-ripgrep
    :doc "Run ripgrep with Projectile"
    :req "ripgrep-0.3.0" "projectile-0.14.0"
    :tag "projectile" "ripgrep"
    :url "https://github.com/nlamirault/ripgrep.el"
    :added "2022-10-31"
    :ensure t
    :after ripgrep projectile))

(defun kb/keymap-prefix (keymap &optional buffer frame)
  "Print KEYMAP prefixes in *Messages* buffer."
  ;; (interactive
  ;;  (let ((v (symbol-at-point))
  ;;        (enable-recursive-minibuffers t)
  ;;        (orig-buffer (current-buffer))
  ;;        val)
  ;;    (setq val (completing-read
  ;;               (format-prompt "Prefix for keymap: " (and (keymapp v) v))
  ;;               #'help--key-binding-keymap))
  ;;    ))
    (map-keymap
     (lambda (key cmd)
       (when (keymapp cmd)
         (message "Prefix: %s" (key-description (vector key)))))
     keymap))

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1"
  :tag "convenience" "project" "emacs>=25.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t
  :blackout  (projectile-mode . '(:eval (when (projectile-project-p) (concat " P:" (projectile-project-name)))))
  :commands projectile-project-p
  :custom ((projectile-indexing-method . 'alien)
           ;; (projectile-completion-system . 'ivy)
           (projectile-enable-caching . t)
           (projectile-sort-order . 'recently-active)
           ;;; Ubuntu provides fd command which isn't fdfind so need to override autodetected value
           ;; (projectile-generic-command . "fdfind . -0 --type f --color=never --strip-cwd-prefix")
           (projectile-fd-executable . "fdfind")
           )
  :bind (:projectile-mode-map
         ;; ("s-p" . projectile-command-map)
         ("C-c p" . 'projectile-command-map))
  :hook ((project-find-functions . project-projectile)
         (magit-mode-hook . kb/add-magit-projects))
  :config
  ;; (setq projectile-mode-map (kbd "C-c p"))
  (defun kb/add-magit-projects ()
    "Add magit repositories to projectile-known-projects."
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)
    (message "***** Projectile: Magit known projects added"))

  (message "**** Configure projectile mode")
  (projectile-mode))

(leaf treemacs-projectile
  :doc "Projectile integration for treemacs"
  :req "emacs-26.1" "projectile-0.14.0" "treemacs-0.0"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :after projectile treemacs)

(leaf python
  :doc "Python's flying circus support for Emacs"
  :tag "builtin"
  :added "2022-10-31"
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :custom ((python-shell-interpreter . "python3")
           (py-python-command-args . '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
           (py-force-py-shell-name-p . t)
           (py-shell-switch-buffers-on-execute-p . t)
           (py-switch-buffers-on-execute-p . t)
           (py-split-windows-on-execute-p . nil)
           (py-smart-indentation . t)
           ))

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :added "2023-06-21"
  :emacs>= 25.1
  :ensure t
  :config
  :hook (rust-mode-hook . #'eglot-ensure))

(leaf flycheck-rust
  :when (package-installed-p 'flycheck)
  :doc "Flycheck: Rust additions and Cargo support"
  :req "emacs-24.1" "flycheck-28" "dash-2.13.0" "seq-2.3" "let-alist-1.0.4"
  :tag "convenience" "tools" "emacs>=24.1"
  :url "https://github.com/flycheck/flycheck-rust"
  :added "2023-06-21"
  :emacs>= 24.1
  :ensure t
  :after flycheck
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  )

;; (leaf rustic
;;   :doc "Rust development environment"
;;   :req "emacs-26.1" "rust-mode-1.0.3" "dash-2.13.0" "f-0.18.2" "let-alist-1.0.4" "markdown-mode-2.3" "project-0.3.0" "s-1.10.0" "seq-2.3" "spinner-1.7.3" "xterm-color-1.6"
;;   :tag "languages" "emacs>=26.1"
;;   :added "2023-06-21"
;;   :emacs>= 26.1
;;   :ensure t
;;   :after rust-mode markdown-mode spinner xterm-color)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-27.1" "compat-30.0.1.0" "dash-2.19.1" "magit-section-4.2.0" "seq-2.24" "transient-0.8.2" "with-editor-3.4.3"
  :tag "vc" "tools" "git" "emacs>=27.1"
  :url "https://github.com/magit/magit"
  :added "2025-01-07"
  :emacs>= 27.1
  :ensure t
  :after compat magit-section with-editor
  :commands magit-list-repos magit-status-quick
  ;; :commands magit-status
  ;; :bind (:map global-map
  ;;          ("C-c n". magit-list-repos))
  :custom ((magit-display-buffer-function . #'magit-display-buffer-same-window-except-diff-v1)
           (magit-repository-directories . '(("~/projects" . 2)))
           (git-commit-summary-max-length . 50)
           )
  :bind (("C-x v SPC" . magit-status)
         ;; ("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status-quick)
         ("C-x M-g" . magit-dispatch)
         ;; (:projectile-keymap-prefix
         ;;  ("m" . project-magit)
         ;; )
         )
  :config
  (put 'magit-clean 'disabled nil))

(leaf treemacs-magit
  :doc "Magit integration for treemacs"
  :req "emacs-26.1" "treemacs-0.0" "pfuture-1.3" "magit-2.90.0"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :after treemacs pfuture magit)

;; =====================================================================
;; https://www.rahuljuliato.com/posts/nerd-fonts
(defun lemacs/all-available-fonts ()
  "Create and visit a buffer containing a sorted list of available fonts."
  (interactive)
  (let ((font-list (sort (x-list-fonts "*") #'string<))
        (font-buffer (generate-new-buffer "*Font List*")))
    (with-current-buffer font-buffer
      (dolist (font font-list)
        (let* ((font-family (nth 2 (split-string font "-"))))
          (insert (format "%s\n" (propertize font 'face `(:family ,font-family :height 110))))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (pop-to-buffer font-buffer)))


;; Helper functions to review GIT Reflog
;; based on article
;; https://www.rahuljuliato.com/posts/vc-git-functions
(defun emacs-solo/vc-git-reflog ()
  "Show git reflog in a new buffer with ANSI colors and custom keybindings."
  (interactive)
  (let* ((root (vc-root-dir))
         (buffer (get-buffer-create "*vc-git-reflog*")))
    (with-current-buffer buffer
      (setq-local vc-git-reflog-root root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (vc-git-command buffer nil nil
                        "reflog"
                        "--color=always"
                        "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
        (goto-char (point-min))
        (ansi-color-apply-on-region (point-min) (point-max)))

      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "/") #'isearch-forward)
        (define-key map (kbd "p") #'previous-line)
        (define-key map (kbd "n") #'next-line)
        (define-key map (kbd "q") #'kill-buffer-and-window)

        (use-local-map map))

      (setq buffer-read-only t)
      (setq mode-name "Git-Reflog")
      (setq major-mode 'special-mode))
    (pop-to-buffer buffer)))

(global-set-key (kbd "C-x v R") 'emacs-solo/vc-git-reflog)

(defun emacs-solo/vc-browse-remote (&optional current-line)
  "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
  (interactive "P")
  (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
         (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
         (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
         (line (line-number-at-pos)))
    (message "Opening remote on browser: %s" remote-url)
    (if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
        (let ((host (match-string 1 remote-url))
              (path (match-string 2 remote-url)))
          ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
          (when (string-prefix-p "git@" host)
            (setq host (replace-regexp-in-string "^git@" "" host)))
          ;; Construct the appropriate URL based on CURRENT-LINE
          (browse-url
           (if current-line
               (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
             (format "https://%s/%s" host path))))
      (message "Could not determine repository URL"))))

(global-set-key (kbd "C-x v B") 'emacs-solo/vc-browse-remote)

(defun emacs-solo/vc-diff-on-current-hunk ()
  "Show the diff for the current file and jump to the hunk containing the current line."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (message "Current line in file: %d" current-line)
    (vc-diff) ; Generate the diff buffer
    (with-current-buffer "*vc-diff*"
      (goto-char (point-min))
      (let ((found-hunk nil))
        (while (and (not found-hunk)
                    (re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
          (let* ((start-line (string-to-number (match-string 2)))
                 (line-count (string-to-number (match-string 3)))
                 (end-line (+ start-line line-count)))
            (message "Found hunk: %d to %d" start-line end-line)
            (when (and (>= current-line start-line)
                       (<= current-line end-line))
              (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
              (setq found-hunk t)
              (goto-char (match-beginning 0)))))
        (unless found-hunk
          (message "Current line %d is not within any hunk range." current-line)
          (goto-char (point-min)))))))

(global-set-key (kbd "C-x v =") 'emacs-solo/vc-diff-on-current-hunk)
;;=====================================================================

(leaf treesit
  :doc "tree-sitter utilities"
  :tag "builtin" "languages" "tree-sitter" "treesit"
  :added "2025-07-23"
  :custom
  (treesit-language-source-alist . '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                     (cmake "https://github.com/uyha/tree-sitter-cmake")
                                     (css "https://github.com/tree-sitter/tree-sitter-css")
                                     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                     (go "https://github.com/tree-sitter/tree-sitter-go")
                                     (html "https://github.com/tree-sitter/tree-sitter-html")
                                     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                     (json "https://github.com/tree-sitter/tree-sitter-json")
                                     (make "https://github.com/alemuller/tree-sitter-make")
                                     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                     (python "https://github.com/tree-sitter/tree-sitter-python")
                                     (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")

                                     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
                                     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                     (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                     (c "https://github.com/tree-sitter/tree-sitter-c")
                                     (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")))
  (treesit-font-lock-level . 4))

(leaf tsc
  :doc "Core Tree-sitter APIs"
  :req "emacs-25.1"
  :tag "tree-sitter" "dynamic-modules" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
  :added "2025-01-27"
  :emacs>= 25.1
  :ensure t)
(leaf tree-sitter
  :doc "Incremental parsing system"
  :req "emacs-25.1" "tsc-0.18.0"
  :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
  :added "2025-01-27"
  :emacs>= 25.1
  :ensure t
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :blackout t
  :preface (defun kb/dd ()
             (with-current-buffer tree-sitter-debug--tree-buffer
               (highlight-indent-guides-mode 1)))
  :advice (:after tree-sitter-debug--setup kb/dd )
  :init
  (setq tree-sitter-debug-jump-buttons  t
        tree-sitter-debug-highlight-jump-region t)

  :config
  (global-tree-sitter-mode)
  ;; (advice-add 'tree-sitter-debug--setup :after #'kb/dd)
  )

(leaf tree-sitter-indent
  :doc "Provide indentation with a Tree-sitter backend"
  :req "emacs-26.1" "tree-sitter-0.12.1" "seq-2.20"
  :tag "internal" "convenience" "emacs>=26.1"
  :url "https://codeberg.org/FelipeLema/tree-sitter-indent.el"
  :added "2025-06-22"
  :emacs>= 26.1
  :ensure t
  :after tree-sitter)
(leaf tree-sitter-langs
  :doc "Grammar bundle for tree-sitter"
  :req "emacs-25.1" "tree-sitter-0.15.0"
  :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/tree-sitter-langs"
  :added "2025-01-27"
  :emacs>= 25.1
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  )
(leaf tree-sitter-ispell
  :doc "Run ispell on tree-sitter text nodes"
  :req "emacs-26.1" "tree-sitter-0.15.0"
  :tag "emacs>=26.1"
  :url "https://github.com/erickgnavar/tree-sitter-ispell.el"
  :added "2025-01-27"
  :emacs>= 26.1
  :ensure t
  :after tree-sitter
  :bind (("C-c C-s" . 'tree-sitter-ispell-run-at-point)))

(leaf js-ts-defs
  :doc "Find JavaScript variable definitions using tree-sitter"
  :req "emacs-29.1"
  :tag "tree-sitter" "javascript" "languages" "emacs>=29.1"
  :url "https://github.com/jacksonrayhamilton/js-ts-defs"
  :added "2025-07-23"
  :emacs>= 29.1
  :ensure t
  :bind ((:js-ts-mode-map
          ("M-." . #'js-ts-defs-jump-to-definition))))

(leaf git-timemachine
  :doc "Walk through git revisions of a file"
  :req "emacs-24.3" "transient-0.1.0"
  :tag "vc" "emacs>=24.3"
  :url "https://gitlab.com/pidu/git-timemachine"
  :added "2022-10-31"
  :emacs>= 24.3
  :ensure t)

(leaf git-link
  :doc "Get the GitHub/Bitbucket/GitLab URL for a buffer location"
  :req "emacs-24.3"
  :tag "convenience" "azure" "aws" "sourcehut" "gitlab" "bitbucket" "github" "vc" "git" "emacs>=24.3"
  :url "http://github.com/sshaw/git-link"
  :added "2025-07-01"
  :emacs>= 24.3
  :ensure t)

(leaf transient-posframe
  :doc "Using posframe to show transient"
  :req "emacs-26.0" "posframe-0.4.3" "transient-0.2.0"
  :tag "tooltip" "bindings" "convenience" "emacs>=26.0"
  :url "https://github.com/yanghaoxie/transient-posframe"
  :added "2022-10-31"
  :emacs>= 26.0
  :ensure t
  :after posframe
  :config (transient-posframe-mode)
  ;; (customize-set-value 'transient-posframe-border-width 1 "customized by me")
  ;; (customize-set-value 'transient-posframe-min-height 100 "customized by me")
  ;; (customize-set-value 'transient-posframe-min-width 100 "customized by me")
  )

;; (leaf ghub
;;   :doc "Client libraries for Git forge APIs"
;;   :req "emacs-29.1" "compat-30.0.2.0" "let-alist-1.0.6" "llama-0.6.1" "treepy-0.1.2"
;;   :tag "tools" "emacs>=29.1"
;;   :url "https://github.com/magit/ghub"
;;   :added "2025-04-18"
;;   :emacs>= 29.1
;;   :ensure t
;;   :after compat llama treepy)

;;--------------------------------------------------------------------------------
;; Optional packages
;;--------------------------------------------------------------------------------

(if (not (package-installed-p 'puni))
    (progn
      "puni not installed: Using builtin paren mode."
      (leaf paren
        :doc "highlight matching paren"
        :tag "builtin"
        :added "2022-10-31"
        :custom ((show-paren-style . 'parenthesis)
                 (show-paren-when-point-inside-paren . t)
                 (show-paren-when-point-in-periphery . t)
                 )
        :config
        (show-paren-mode 1)))
  )


(leaf puni
  :disabled t
  :doc "Parentheses Universalistic"
  :req "emacs-26.1"
  :tag "tools" "lisp" "convenience" "emacs>=26.1"
  :url "https://github.com/AmaiKinono/puni"
  :added "2025-06-12"
  :emacs>= 26.1
  :ensure t
  :require t
  :config
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)

 ;;  :bind (("%" . kb/puni-match-parenthesis))

 ;;  :config
 ;;  (defun kb/puni-match-parenthesis (arg)
 ;;    "Match the current character according to the syntax table.

 ;; Based on the freely available match-paren.el by Kayvan Sylvan.
 ;; I merged code from goto-matching-paren-or-insert and match-it.

 ;; When ARG does not belong to matching pair then insert it at point.

 ;; You can define new \"parentheses\" (matching pairs).
 ;; Example: angle brackets.  Add the following to your .emacs file:

 ;; (modify-syntax-entry ?< \"(>\" )
 ;; (modify-syntax-entry ?> \")<\" )

 ;; You can set hot keys to perform matching with one keystroke.
 ;; Example: f6 and Control-C 6.

 ;; (global-set-key \"\\C-c6\" 'match-parenthesis)
 ;; (global-set-key [f6] 'match-parenthesis)

 ;; Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
 ;;    (interactive "p")
 ;;    (let
 ;;        ((syntax (puni--syntax-char-after)))
 ;;      (cond
 ;;       ((= syntax ?\()
 ;;        ;; (puni-end-of-list-around-point)
 ;;        ;; (puni-up-list)
 ;;        ;; (146259 . 146289)
 ;;        ;; (goto-char (puni-end-pos-of-list-around-point))
 ;;        ;; (puni-strict-forward-sexp)
 ;;        ;; (puni-bounds-of-sexp-at-point)
 ;;        ;; (puni-forward-sexp-or-up-list)
 ;;        (puni--forward-same-char)
 ;;        (puni--forward-same-syntax (puni-end-pos-of-list-around-point))
 ;;        ;; (puni-beginning-of-list-around-point)
 ;;        )
 ;;       ((= syntax ?\))
 ;;        ;; (goto-char (puni-beginning-pos-of-list-around-point))
 ;;        ;; (puni-beginning-of-list-around-point)
 ;;        ;; (puni-end-of-list-around-point)
 ;;        ;; (puni-backward-sexp-or-up-list)
 ;;        ;; (puni-strict-backward-sexp)
 ;;        (puni-backward-sexp-or-up-list)
 ;;        (puni--backward-same-syntax)
 ;;        )
 ;;       (t (self-insert-command (or arg 1))) ) )
)

(leaf smartparens
  :disabled t
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs"
  :req "dash-2.13.0"
  :tag "editing" "convenience" "abbrev"
  :url "https://github.com/Fuco1/smartparens"
  :added "2025-01-16"
  :ensure t
  ;; :bind
  ;; (:map smartparens-mode-map
  ;; (
  ;;   ("C-M-f" . sp-forward-sexp)
  ;;   ("C-M-b" . sp-backward-sexp)
  ;;   ("C-M-<right>" . sp-forward-sexp)
  ;;   ("C-M-<left>" . sp-backward-sexp)

  ;;   ("M-F" . sp-forward-sexp)
  ;;   ("M-B" . sp-backward-sexp)

  ;;   ("C-M-d" . sp-down-sexp)
  ;;   ("C-M-a" . sp-backward-down-sexp)
  ;;   ("C-S-d" . sp-beginning-of-sexp)
  ;; ("C-S-a" . sp-end-of-sexp)

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

  ;;   ("M-D" . sp-splice-sexp)
  ;; )
  ;; :map emacs-lisp-mode-map (";" . sp-comment)
  ;; :map smartparens-strict-mode-map ([remap c-electric-backspace] . sp-backward-delete-char)
  ;; )
  ;; :hook
  ;; ((minibuffer-setup . turn-on-smartparens-strict-mode)
  ;;  (c-mode-common-hook . (lambda () (require 'smartparens-c)))
  ;;  (org-mode . (lambda () (require 'smartparens-org))))
  ;; :config
  ;; (setq sp-base-key-bindings 'dup)
  :custom ((sp-base-key-bindings . 'sp)
           (sp-autoskip-closing-pair . 'always)
           (sp-hybrid-kill-entire-symbol . 'nil)
           (sp-navigate-interactive-always-progress-point . t))
  ;; :require smartparens-config
  ;; :defvar smartparens-mode-map
  :config
  (defun kb/sp-match-parenthesis (arg)
    "Match the current character according to the syntax table.

Based on the freely available match-paren.el by Kayvan Sylvan.
I merged code from goto-matching-paren-or-insert and match-it.

When ARG does not belong to matching pair then insert it at point.

You can define new \"parentheses\" (matching pairs).
Example: angle brackets.  Add the following to your .emacs file:

(modify-syntax-entry ?< \"(>\" )
(modify-syntax-entry ?> \")<\" )

You can set hot keys to perform matching with one keystroke.
Example: f6 and Control-C 6.

(global-set-key \"\\C-c6\" 'match-parenthesis)
(global-set-key [f6] 'match-parenthesis)

Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
    (interactive "p")
    (let
        ((syntax (sp-get-buffer-char-syntax)))
      (cond
       ((= syntax ?\()
        (sp-forward-sexp) (sp-backward-down-sexp))
       ((= syntax ?\))
        (sp-backward-up-sexp))
       (t (self-insert-command (or arg 1)))
       )
      ))
  :config
  (electric-pair-mode -1)
  ;; (smartparens-global-strict-mode 1)
  (show-smartparens-global-mode t)
  (smartparens-global-mode 1)

  :bind (("C-S-a" . sp-end-of-sexp)
         ("%" . kb/sp-match-parenthesis)))
(leaf jira-markup-mode
  :doc "Emacs Major mode for JIRA-markup-formatted text files"
  :tag "markup" "jira"
  :url "https://github.com/mnuessler/jira-markup-mode"
  :added "2025-01-23"
  :ensure t
  :mode "\\.jira")
(leaf org-jira
  :doc "Syncing between Jira and Org-mode"
  :req "emacs-24.5" "cl-lib-0.5" "request-0.2.0" "dash-2.14.1"
  :tag "tracker" "bug" "org" "jira" "ahungry" "emacs>=24.5"
  :url "https://github.com/ahungry/org-jira"
  :added "2025-01-23"
  :emacs>= 24.5
  :ensure t
  :after org
  :config
  (when (not (file-exists-p "~/.org-jira"))
    (make-directory "~/.org-jira"))
  (setopt jiralib-use-restapi t)
  (setopt jiralib-url (concat "https://" (secrets-get-attribute "default" "CC-JIRA" :host)))
  (setopt jiralib-host (secrets-get-attribute "default" "CC-JIRA" :host))
  (setopt jiralib-user (secrets-get-attribute "default" "CC-JIRA" :user))
  (setopt jiralib-use-PAT t)
  (setopt jiralib-token (cons "Authorization" (concat "Bearer " (secrets-get-secret "default" "CC-JIRA")))))

(leaf jira
  :doc "Emacs Interface to Jira"
  :req "emacs-29.1" "request-0.3.0" "tablist-1.0" "transient-0.8.3" "magit-section-4.2.0"
  :tag "emacs>=29.1"
  :url "https://github.com/unmonoqueteclea/jira.el"
  :added "2025-07-15"
  :emacs>= 29.1
  :ensure t
  :after tablist magit-section
  :config
  (setq jira-base-url (concat "https://" (secrets-get-attribute "default" "CC-JIRA" :host))) ;; Jira instance URL
  ;; (setq jira-username (secrets-get-attribute "default" "CC-JIRA" :user)) ;; Jira username (usually, an email)
  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  ;; (setq jira-token (secrets-get-secret "default" "CC-JIRA"))
  (setq jira-token-is-personal-access-token t)
  (setq jira-api-version 2) ;; Version 2 is also allowed
  ;; ;; (Optional) API token for JIRA TEMPO plugin
  ;; ;; See https://apidocs.tempo.io/
  ;; (setq jira-tempo-token "foobar123123")
  ;; (setq jira-debug t)
  ;; (setq request-log-level 'blather)
  ;; (setq request-message-level 'blather)
  )

(leaf copy-as-format
  :doc "Copy buffer locations as GitHub/Slack/JIRA etc... formatted code"
  :req "cl-lib-0.5"
  :tag "convenience" "tools" "whatsapp" "asciidoc" "rst" "pod" "org-mode" "bitbucket" "gitlab" "telegram" "jira" "slack" "github"
  :url "https://github.com/sshaw/copy-as-format"
  :added "2025-01-23"
  :ensure t
  :custom
  (copy-as-format-asciidoc-include-file-name . t)
  (copy-as-format-include-line-number . t)
  :bind
  (("C-c w o" . copy-as-format-org-mode)
   ("C-c w j" . copy-as-format-org-mode)))

(unless (executable-find "dot") (warn "Command: `dot` not found in system"))
(leaf graphviz-dot-mode
  :doc "Mode for the dot-language used by graphviz (att)."
  :req "emacs-25.0"
  :tag "att" "graphs" "graphviz" "dotlanguage" "dot-language" "dot" "mode" "emacs>=25.0"
  :url "https://ppareit.github.io/graphviz-dot-mode/"
  :added "2022-10-31"
  :when (executable-find "dot")
  :emacs>= 25.0
  :ensure t
  :custom ((graphviz-dot-indent-width . 4))
  :config
  (when (executable-find "xdot")
    (customize-set-variable 'graphviz-dot-view-command "xdot %s")))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :url "http://github.com/joaotavora/yasnippet"
  :added "2022-10-31"
  :ensure t
  ;; :after hydra
  ;; :defines (yas-snippet-dirs yas-active-snippets)
  ;; :pin "melpa"
  :when (package-installed-p 'yasnippet)
  :custom
  (yas-prompt-functions . '(yas/ido-prompt yas/completing-prompt))
  (yas-verbosity . 1)
  (yas-wrap-around-region . t)
  :hook (term-mode-hook . (lambda() (setq yas-dont-activate-functions t)))
  :bind ((:yas-keymap
          ("<return>" . yas-exit-all-snippets)
          ("C-e" . yas/goto-end-of-active-field)
          ("C-a" . yas/goto-start-of-active-field))
         (:yas-minor-mode-map ("<f2>" . hydra-yas/body)))
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name (concat user-emacs-directory "rc/snippets")))
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

;;    :hydra (hydra-yas (:color blue :hint nil)
;;  	            "
;;        ^YASnippets^
;;  --------------------------------------------
;;  Modes:    Load/Visit:    Actions:
;;
;;  _g_lobal  _d_irectory    _i_nsert
;;  _m_inor   _f_ile         _t_ryout
;;  _e_xtra   _l_ist         _n_ew
;;   _a_ll
;;  "
;;                  ("d" yas-load-directory)
;;                  ("e" yas-activate-extra-mode)
;;                  ("i" yas-insert-snippet)
;;                  ("f" yas-visit-snippet-file :color blue)
;;                  ("n" yas-new-snippet)
;;                  ("t" yas-tryout-snippet)
;;                  ("l" yas-describe-tables)
;;                  ("g" yas/global-mode)
;;                  ("m" yas/minor-mode)
;;                  ("a" yas-reload-all))
;;    :config
  (leaf yasnippet-snippets
    :doc "Collection of yasnippet snippets"
    :req "yasnippet-0.8.0"
    :tag "snippets"
    :url "https://github.com/AndreaCrotti/yasnippet-snippets"
    :added "2022-10-31"
    :ensure t
    :after yasnippet)
  (leaf yasnippet-classic-snippets
    :doc "\"Classic\" yasnippet snippets"
    :req "yasnippet-0.9.1"
    :tag "snippets"
    :url "http://elpa.gnu.org/packages/yasnippet-classic-snippets.html"
    :added "2022-10-31"
    :ensure t
    :after yasnippet)
  )

(defcustom kb/plantuml-jar-path (expand-file-name "~/.java/libs/plantuml.jar")
  "Location where to search for plantuml.jar file.

Download and put appropriate file there."
  :type 'file
  :group 'kb-config)

(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :req "dash-2.0.0" "emacs-25.0"
  :tag "ascii" "plantuml" "uml" "emacs>=25.0"
  :added "2022-10-31"
  :emacs>= 25.0
  :ensure t
  :custom ((plantuml-jar-path . kb/plantuml-jar-path)
           (plantuml-default-exec-mode . 'jar)
           (plantuml-indent-level . 4)
           (org-plantuml-jar-path . kb/plantuml-jar-path))
  :hook (plantuml-mode-hook . (lambda ()
                                (set-fill-column 100)
                                (display-fill-column-indicator-mode)
                                (add-to-list 'org-babel-load-languages '((plantuml . t)))
                                (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
                                ))
  :bind (:plantuml-mode-map
         ("C-c C-p" . plantuml-preview-buffer))
  :config
  (message "**** Configure plantuml-mode")
  (plantuml-set-output-type "svg")
  (if (not (file-exists-p kb/plantuml-jar-path))
      (warn (format "PlantUML JAR (%s) not found. Download by running '(plantuml-download-jar) function." kb/plantuml-jar-path))
    )
  :config
  (leaf flycheck-plantuml
    :when (package-installed-p 'flycheck)
    :doc "Integrate plantuml with flycheck"
    :req "flycheck-0.24" "emacs-24.4" "plantuml-mode-1.2.2"
    :tag "emacs>=24.4"
    :url "https://github.com/alexmurray/flycheck-plantuml"
    :added "2022-10-31"
    :emacs>= 24.4
    :ensure t
    :after flycheck plantuml-mode
    :config (flycheck-plantuml-setup)
    )
  )

(leaf flymake-jslint
  :doc "A flymake handler for javascript using jslint"
  :req "flymake-easy-0.1"
  :url "https://github.com/purcell/flymake-jslint"
  :added "2025-07-07"
  :ensure t
  :hook ((js-mode-hook js-ts-mode-hook) . flymake-jslint-load))



(leaf json-snatcher
  :doc "Grabs the path to JSON values in a JSON file"
  :req "emacs-24"
  :tag "emacs>=24"
  :url "http://github.com/sterlingg/json-snatcher"
  :added "2025-01-27"
  :emacs>= 24
  :ensure t
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin."
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  :hook
  ((js-mode-hook js2-mode-hook js-ts-mode-hook) . js-mode-bindings))
(leaf json-mode
  :doc "Major mode for editing JSON files."
  :req "json-snatcher-1.0.0" "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/joshwnj/json-mode"
  :added "2022-11-01"
  :emacs>= 24.4
  :ensure t
  :after json-snatcher
  ;; :config
  )

(leaf web-mode
  :doc "Major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :url "https://web-mode.org"
  :added "2025-07-11"
  :emacs>= 23.1
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  ((web-mode-markup-indent-offset             . 2)
   (web-mode-css-indent-offset                . 2)
   (web-mode-code-indent-offset               . 2)
   (web-mode-block-padding                    . 2)
   (web-mode-comment-style                    . 2)
   (web-mode-enable-css-colorization          . t)
   (web-mode-enable-auto-pairing              . t)
   (web-mode-enable-comment-keywords          . t)
   (web-mode-enable-current-element-highlight . t))

  :hook (web-mode-hook . kb/web-mode-config)
  :config
  (defun kb/web-mode-config ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      ;; (tide-setup)
      ;; (tide-hl-identifier-mode)
      (flymake-mode)
      (eldoc-mode)
      (eglot-ensure)
      )))

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :url "http://github.com/ananthakumaran/typescript.el"
  :added "2025-07-11"
  :emacs>= 24.3
  :ensure t
  :custom (typescript-indent-level . 2)
  :hook ((typescript-mode-hook . subword-mode)
         (typescript-mode-hook . flymake-mode)))

(leaf prettier-js
  :doc "Minor mode to format code on file save"
  :req "emacs-28.1"
  :tag "js" "edit" "wp" "convenience" "emacs>=28.1"
  :url "https://github.com/prettier/prettier-emacs"
  :added "2025-07-11"
  :emacs>= 28.1
  :ensure t
  :hook ((typescript-mode-hook . prettier-js-mode)
         (web-mode-hook . prettier-js-mode)))

(leaf prettier
  :doc "Code formatting with Prettier"
  :req "emacs-26.1" "iter2-0.9" "nvm-0.2" "editorconfig-0.9"
  :tag "files" "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/jscheid/prettier.el"
  :added "2025-07-11"
  :emacs>= 26.1
  :ensure t
  ;; :after iter2 nvm editorconfig
  :hook ((web-mode-hook typescript-mode-hook) . prettier-mode) ; json-mode-hook
  ;; :config
  ;; (require 'prettier)
  )

(when (not (executable-find "jsonlint")) (message "jsonlint not found. Can be installed with `npm install jsonlint -g'"))
(leaf flymake-json
  :doc "A flymake handler for json using jsonlint"
  :req "flymake-easy-0.1"
  :url "https://github.com/purcell/flymake-json"
  :added "2025-07-07"
  :ensure t
  ;; :hook (json-mode-hook . flymake-json-load)
)

(defun kb/nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn')t error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(leaf nxml-mode
  :doc "a new XML mode"
  :tag "builtin" "xml" "languages" "hypermedia" "wp"
  :added "2023-08-30"
  :config
  (defun xml-find-file-hook ()
    (when (derived-mode-p 'nxml-mode)
      (which-function-mode t)
      (setq which-func-mode t)
      (add-hook 'which-func-functions 'kb/nxml-where t t)))

  (add-hook 'find-file-hook 'xml-find-file-hook t))

(leaf gt
  :doc "Translation framework, configurable and scalable"
  :req "emacs-28.1" "pdd-0.2.3"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/lorniu/gt.el"
  :added "2025-07-29"
  :emacs>= 28.1
  :ensure t
  :after pdd
  :custom ((gt-langs . '(en de ja ko pt pl fr zh)))
  :config
  (setq (gt-default-translator
         (gt-translator
          :taker (gt-taker :text 'buffer :pick 'paragraph :langs '(en de ja ko pt pl fr zh))
          :engines (list (gt-google-engine) (gt-bing-engine) (gt-deepl-engine))
          :render (gt-buffer-renderer))))

  (setq gt-preset-translators
      `((ts-quick-de-en . ,(gt-translator
                  :taker (gt-taker :langs '(de en) :text 'word)
                  :engines (gt-bing-engine)
                  :render (gt-overlay-render)))
        (ts-quick-en-pl . ,(gt-translator
                            :taker (gt-taker :langs '(en pl) :text 'word)
                            :engines (gt-deepl-engine)
                            :render (gt-overlay-render)))
        (ts-quick-ja-en . ,(gt-translator
                            :taker (gt-taker :langs '(ja en) :text 'word)
                            :engines (gt-bing-engine)
                            :render (gt-overlay-render)))
        (ts-in-place . ,(gt-translator
                         :taker (gt-taker :langs '(en de ja ko) :text 'sentence)
                         :engines (gt-google-engine)
                         :render (gt-insert-render)))
        (ts-3 . ,(gt-translator
                  :taker (gt-taker :langs '(en de) :text 'buffer
                                   :pick 'word :pick-pred (lambda (w) (length> w 6)))
                  :engines (gt-google-engine)
                  :render (gt-overlay-render :type 'help-echo)))))
  :bind (("<f5>" . #'gt-translate)))

(leaf emamux
  :doc "Interact with tmux"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/syohex/emacs-emamux"
  :added "2023-02-07"
  :emacs>= 24.3
  :ensure t)
(leaf vterm
  :doc "Fully-featured terminal emulator"
  :req "emacs-25.1"
  :tag "terminals" "emacs>=25.1"
  :url "https://github.com/akermu/emacs-libvterm"
  :added "2023-05-09"
  :emacs>= 25.1
  :ensure t)
(leaf password-generator
  :doc "Password generator for humans. Good, Bad, Phonetic passwords included"
  :url "http://github.com/vandrlexay/emacs-password-genarator"
  :added "2025-02-07"
  :ensure t)

;; (leaf auth-source
;;   ;; prefer encrypted auth source to non-encrypted
;;   :custom
;;   (auth-sources . '("~/.emacs.d/secrets/.authinfo.gpg" "~/.authinfo.gpg" "~/.authinfo" "~/.netrc"
;;                     (:source
;;                      (:secrets default))
;;                     "secrets:session" "secrets:Login" default))
;;   )

(require 'auth-source)
(setopt auth-sources '("~/.emacs.d/secrets/.authinfo.gpg" "~/.authinfo.gpg" "~/.authinfo" "~/.netrc"
                       (:source
                        (:secrets default))
                       "secrets:session" "secrets:Login" default))

(leaf gptai
  :disabled t
  :doc "Integrate with the OpenAI API"
  :req "emacs-24.1"
  :tag "convenience" "comm" "emacs>=24.1"
  :url "https://github.com/antonhibl/gptai"
  :added "2023-03-07"
  :emacs>= 24.1
  :ensure t
  :require t)

(leaf haskell-mode
  :doc "A Haskell editing mode"
  :req "emacs-25.1"
  :tag "haskell" "files" "faces" "emacs>=25.1"
  :url "https://github.com/haskell/haskell-mode"
  :added "2023-03-08"
  :emacs>= 25.1
  :ensure t
  ;; :hook ((haskell-mode-hook . turn-on-haskell-indentation)
  ;;        (haskell-mode-hook . interactive-haskell-mode))
  :mode "\\.hs\\'"
  :bind (:haskell-mode-map
         ("C-," . #'haskell-move-nested-left)
         ("C-." . #'haskell-move-nested-right)
         ("<f7>" . #'haskell-navigate-imports)
         ("C-c C-g" . #'haskell-interactive-bring)
         ("C-c C-l" . #'haskell-process-load-or-reload)
         ("C-c C-t" . #'haskell-process-do-type)
         ("C-c C-i" . #'haskell-process-do-info)
         ("C-c C-c" . #'haskell-process-cabal-build)
         ("C-c C-k" . #'haskell-interactive-mode-clear)
         ("C-c c" . #'haskell-process-cabal)
         ("M-n" . haskell-goto-next-error)
         ("M-p" . haskell-goto-prev-error)
         ;; ("<SPC>" . #'haskell-mode-contextual-space)
         )
  :custom ((haskell-stylish-on-save . t)
           (haskell-process-type . 'cabal-repl)
           (haskell-process-suggest-remove-import-lines . t)
           (haskell-process-auto-import-loaded-modules . t)
           (haskell-process-log . t)
           )
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . haskell-indent-mode)
         (haskell-mode . haskell-setup-outline-mode))
  :config
  (defun haskell-setup-outline-mode ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-"))

  ;; (setq haskell-ghci-program-name "cabal")
  ;; (setq haskell-ghci-prgram-args '("repl"))
  )

;; (leaf ghci-completion
;;   :doc "Completion for GHCi commands in inferior-haskell buffers"
;;   :req "emacs-24.1" "cl-lib-0.5"
;;   :tag "convenience" "emacs>=24.1"
;;   :added "2023-03-13"
;;   :emacs>= 24.1
;;   :ensure t
;;   :config
;;   (setq haskell-ghci-program-name "cabal")
;;   (setq haskell-ghci-program-args '("repl"))
;;   )

;; (leaf copilot
;;   :req "emacs-27.2" "editorconfig-0.8.2" "jsonrpc-1.0.14" "f-0.20.0"
;;   :tag "copilot" "convenience" "emacs>=27.2"
;;   :url "https://github.com/copilot-emacs/copilot.el"
;;   :added "2025-07-25"
;;   :emacs>= 27.2
;;   :ensure t
;;   :after editorconfig jsonrpc
;;   :hook (prog-mode-hook . copilot-mode))

;; (leaf copilot-chat
;;   :disabled t
;;   :doc "Copilot chat interface"
;;   :req "request-0.3.2" "markdown-mode-2.6" "emacs-27.1" "chatgpt-shell-1.6.1" "magit-4.0.0"
;;   :tag "tools" "convenience" "emacs>=27.1"
;;   :url "https://github.com/chep/copilot-chat.el"
;;   :added "2024-11-18"
;;   :emacs>= 27.1
;;   :ensure t
;;   :after markdown-mode chatgpt-shell magit
;;   :hook
;;   (copilot-chat-insert-commit-message . git-commit-setup-hook)
;;   ;; :custom
;;   ;; (copilot-chat-frontend . 'markdown)
;;   )

;; (leaf gptai
;;   :doc "Integrate with the OpenAI API"
;;   :req "emacs-24.1"
;;   :tag "convenience" "comm" "emacs>=24.1"
;;   :url "https://github.com/antonhibl/gptai"
;;   :added "2025-07-25"
;;   :emacs>= 24.1
;;   :ensure t
;;   :config
;;   ;; set standard configurations
;;   (setopt gptai-model "gpt-3.5-turbo")
;;   (setopt gptai-api-key (secrets-get-secret "default" "ChatGPT"))

;;   ;; set keybindings optionally
;;   (global-set-key (kbd "C-c o") 'gptai-send-query))


;; (leaf claudia
;;   :disabled t
;;   :blackout claudia-mode
;;   :doc "Claude AI integration"
;;   :req "emacs-29.1" "uuidgen-0.3" "markdown-mode-2.3"
;;   :tag "codegen" "productivity" "tools" "ai" "emacs>=29.1"
;;   :url "https://github.com/mzacho/claudia"
;;   :added "2025-07-21"
;;   :emacs>= 29.1
;;   :ensure t
;;   :after uuidgen markdown-mode
;;   :custom
;;   ;; Claude.ai session key and organization
;;   ((claudia-session-key (secrets-get-attribute "default" "ClaudAI" :session))
;;    (claudia-organization-id (secrets-get-attribute "default" "ClaudAI" :organization)))
;;   ;; Anthropic API session key
;;   ;; (claudia-anthropic-api-key "sk-ant-api03-...")

;;   ;; Ignore most buffers not containing code
;;   (claudia-ignore-buffers-regexps '("\\*.*\\*" "magit.*"))
;;   (claudia-ignore-buffers-major-mode-regexps '("dired-mode" "pdf-view-mode"))

;;   ;; I find that 100000 characters of code is about 40% of the allowed project knowledge
;;   (claudia-max-recent-buffers nil)
;;   (claudia-max-recent-buffers-content-length 100000)

;;   ;; Instruct Claude.ai to deliver concise responses formatted in markdown
;;   (claudia-default-project-prompt-template
;;       (string-join
;;        (list claudia-instruction-initial
;;              claudia-instruction-markdown
;;              claudia-instruction-confirmation)
;;        " [NEXT INSTRUCTION] "))


;;   ;; claudia-model: Claude model to use (default: "claude-3-5-sonnet-20240620")
;;   ;; claudia-chat-display-buffer: Whether to display chat buffer on response (default: t)
;;   ;; claudia-max-recent-buffers: Maximum number of recent buffers to track (default: 3)
;;   ;; claudia-max-recent-buffers-content-length: Maximum total content length for tracked buffers
;;   ;; claudia-ignore-buffers-regexps: List of buffer name patterns to ignore
;;   ;; claudia-ignore-buffers-major-mode-regexps: List of major modes to ignore
;;   ;; claudia-default-project-prompt-template: New projects are created with this template
;;   :bind (("C-x c RET" . claudia-prompt)
;;          ("C-x c c" . claudia-select-or-create-chat)
;;          ("C-x c p" . claudia-select-or-create-project)
;;          ("C-x c d" . claudia-delete-project-knowledge))
;;   :config
;;   ;; My default project and chat - set these values to your own
;;   ;; default project and chat-ids after creating your first chat
;;   ;; (setq claudia--current-project "7dfeb125-3269-431e-b0c4-d11d85936732")
;;   ;; (setq claudia--current-chat "deed76c0-3d24-4055-ab0a-c639868c2afb")
;;   (claudia-mode))

(leaf uuidgen
  :doc "Provides various UUID generating functions"
  :tag "tools" "lisp" "extensions"
  :added "2023-08-10"
  :ensure t)

(leaf time-uuid-mode
  :doc "Minor mode for previewing time uuids as an overlay"
  :req "emacs-24.3"
  :tag "tools" "data" "convenience" "extensions" "emacs>=24.3"
  :url "https://github.com/RobertPlant/time-uuid-mode"
  :added "2023-08-10"
  :emacs>= 24.3
  :ensure t)

(message "** Init finished")
;;; init.el ends here
