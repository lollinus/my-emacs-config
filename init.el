;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix; lexical-binding: t; -*-
;;; Commentary:
;; Load all configuration parts

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

;;; Code:
;; don't let Customize mess with my .emacs
(defconst rc-directory (expand-file-name "rc" user-emacs-directory)
  "Variable storing path to RC directory.
By default is subdirectory of `user-emacs-directory'.")
(message "Adding `%S' to `load-path'" rc-directory)
(add-to-list 'load-path rc-directory)

(when (eq system-type 'windows-nt)
  (setopt w32-pipe-read-delay '-1)
  )

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
;; (global-hl-line-mode -1)

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


;;--------------------------------------------------------------------------------
;; Elpa setup
;;--------------------------------------------------------------------------------
;; (require 'cl-lib)
;; (defvar kb/package-archives ()
;;   "Set of achives which will be finally set to package-achives.")

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this  warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'kb/package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   (add-to-list 'kb/package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)

;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'kb/package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
;;     )
;;   )

;; (customize-set-variable
;;  'package-archives '(("org" . "https://orgmode.org/elpa/")
;;                      ("melpa" . "https://melpa.org/packages/")
;;                      ("gnu" . "https://elpa.gnu.org/packages/")
;; 		     ))
;; (package-initialize)

;; ========================= straight bootstrap ========================
(defvar straight-bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomesage))

(setopt straight-enable-package-integration t)
(setopt straight-vc-git-default-clone-depth 1)
(message "straight bootstrap done")
;; ========================= straight bootstrap ========================

;; ========================== el-get bootstrap =========================
(message "el-get bootstrap")

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)
(message "el-get bootstrap done")
;; ========================== el-get bootstrap =========================

;; =========================== leaf bootstrap ==========================
;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
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

;; (leaf el-get
;;   :doc "Manage the external elisp bits and pieces you depend upon."
;;   :tag "emacswiki" "http-tar" "http" "pacman" "fink" "apt-get" "hg" "darcs" "svn" "cvs" "bzr" "git-svn" "git" "elpa" "install" "elisp" "package" "emacs"
;;   :url "http://www.emacswiki.org/emacs/el-get"
;;   :added "2025-03-11"
;;   :ensure t)
(leaf hydra-posframe
  ;; :disabled t
  :el-get "Ladicle/hydra-posframe"
  ;; :init (require 'el-get)
  ;; :unless (fboundp 'hydra-posframe-mode)
  ;; (el-get-bundle "Ladicle/hydra-posframe")
  ;; (autoload #'hydra-posframe-mode "hydra-posframe" nil t))
  :hook (after-init-hook . hydra-posframe-mode))

;; (leaf async-await :el-get chuntaro/emacs-async-await)
;; (leaf promise :el-get chuntaro/emacs-promise)
;; (leaf iter2 :el-get doublep/iter2)
;; (leaf async :el-get jwiegley/emacs-async)

;; load no-littering as soon as possible during init so it can hook as many
;; paths as possible
(leaf no-littering
  :doc "Help keeping ~/.config/emacs clean."
  :req "emacs-26.1" "compat-30.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/emacscollective/no-littering"
  :added "2025-06-09"
  :emacs>= 26.1
  :ensure t
  :after compat
  :require recentf no-littering
  :defvar (no-littering-var-directory no-littering-etc-directory)
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

;; (leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

(leaf dashboard
  :doc "A startup screen extracted from Spacemacs"
  :req "emacs-27.1"
  :tag "dashboard" "tools" "screen" "startup" "emacs>=27.1"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :added "2025-02-27"
  :emacs>= 27.1
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (when (featurep 'projectile)
    (setopt dashboard-projects-backend 'projectile))
  (setopt initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq dashboard-items '((projects . 10)
                          (recents . 10)
                          (bookmarks . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))


  ;; (setopt dashboard-icon-type (cond ((featurep 'all-the-icons)
  ;;        ('all-the-icons))
  ;;       ((featurep 'nerd-icons)
  ;;        (setopt dashboard-icon-type 'nerd-icons))
  ;;       (t (message "Icon package not detected")))

  ;; (setq dashboard-display-icons-p t)
  ;; (setq dashboard-icon-type 'nerd-icons)
  ;; (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)

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
           (transient-mark-mode . nil)
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
  :bind (("M-o" . other-window)))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((large-file-warning-threshold . 100000000)
           (mode-require-final-newline . t)      ; add a newline to end of file)
           (make-backup-files . nil)
           ))

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
           (display-time-default-load-average . 15))
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
(define-key global-map (kbd "C-c d") 'kb/set-buffer-eol-dos)
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
  :disabled t
  :doc "Copy file name into kill ring"
  :req "emacs-26"
  :tag "files" "emacs>=26"
  :url "https://github.com/chyla/kill-file-path/kill-file-path.el"
  :added "2024-02-19"
  :emacs>= 26
  :ensure t
  :bind
  ("C-c f" . kill-file-path))

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
(leaf show-eol
  :doc "Show end of line symbol in buffer"
  :req "emacs-24.4"
  :tag "line" "eol" "end" "convenience" "emacs>=24.4"
  :url "https://github.com/jcs-elpa/show-eol"
  :added "2025-06-06"
  :emacs>= 24.4
  :ensure t)

;; Set Theme depending if emacs frame is inside TTY o GUI
;;--------------------------------------------------------------------------------
(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "faces" "themes" "emacs>=25.1"
  :url "https://github.com/doomemacs/themes"
  :added "2022-11-04"
  ;; :emacs>= 25.1
  :ensure t
  ;; :defines (doom-themes-treemacs-theme)
  :after all-the-icons ;; doom-atom requires all-the-icons
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  (doom-themes-treemacs-theme . "doom-atom")
  :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-xcode t)
  ;; (when (fboundp doom-dark+-blue-modeline)
  ;; (setq doom-dark+-blue-modeline t)
  ;; (setq doom-dark+-padded-modeline nil)
  ;; )
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
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
  :require t
  :config
  (solaire-global-mode))
;; (leaf modus-themes
;;   :doc "Elegant, highly legible and customizable themes"
;;   :req "emacs-27.1"
;;   :tag "accessibility" "theme" "faces" "emacs>=27.1"
;;   :url "https://git.sr.ht/~protesilaos/modus-themes"
;;   :added "2023-02-10"
;;   :emacs>= 27.1
;;   :ensure t)
(leaf ember-twilight-theme
  :doc "Ember Twilight theme"
  :req "emacs-24.1"
  :tag "dark" "twilight" "ember" "theme" "faces" "emacs>=24.1"
  :url "https://github.com/madara123pain/unique-emacs-theme-pack"
  :added "2025-03-11"
  :emacs>= 24.1
  :ensure t)
(leaf marron-gold-theme
  :doc "A rich marron-gold theme"
  :req "emacs-24.1"
  :tag "elegant" "warm" "gold" "marron" "theme" "faces" "emacs>=24.1"
  :url "https://github.com/madara123pain/unique-emacs-theme-pack"
  :added "2025-03-11"
  :emacs>= 24.1
  :ensure t)
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
(leaf solarized-gruvbox-theme
  :doc "Solarized Gruvbox theme"
  :req "emacs-24.1"
  :tag "dark" "gruvbox" "solarized" "theme" "faces" "emacs>=24.1"
  :url "https://github.com/madara123pain/unique-emacs-theme-pack"
  :added "2025-03-11"
  :emacs>= 24.1
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
  :doc "A dark, medium contrast theme for Emacs"
  :req "all-the-icons-2.0.0"
  :tag "themes"
  :url "https://github.com/NicolasPetton/zerodark-theme"
  :added "2025-06-16"
  :after all-the-icons
  :require t
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
      (message (format "Theme %S activated" theme))
      ))
  (add-hook 'enable-theme-functions #'kb/zerodark-theme-enabled))

(leaf doom-modeline
  :hook window-setup-hook
  :ensure t
  :custom (
           (doom-modeline-hud . t)
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

(leaf all-the-icons-nerd-fonts
  :disabled t
  :doc "Nerd font integration for all-the-icons"
  :req "emacs-28.1" "all-the-icons-5.0" "nerd-icons-0.0.1"
  :tag "lisp" "convenience" "emacs>=28.1"
  :url "https://github.com/mohkale/all-the-icons-nerd-fonts"
  :added "2025-06-02"
  :emacs>= 28.1
  :ensure t
  :after all-the-icons nerd-icons
  :config
  (all-the-icons-nerd-fonts-prefer)
  ;; (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 105)
  )

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library. Required by doom-modeline"
  :req "emacs-24.3"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :added "2023-06-15"
  :emacs>= 24.3
  :ensure t
  :config
  ;; (unless (file-exists-p (expand-file-name (concat (nerd-icons-fonts-subdirectory nerd-icons-font-names))))
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/NFM.ttf"))
    (nerd-icons-install-fonts t)))

(leaf nerd-icons-ibuffer
  :doc "Display nerd icons in ibuffer"
  :req "emacs-24.3" "nerd-icons-0.0.1"
  :tag "ibuffer" "icons" "convenience" "emacs>=24.3"
  :url "https://github.com/seagle0128/nerd-icons-ibuffer"
  :added "2023-06-15"
  :emacs>= 24.3
  :ensure t
  :after nerd-icons
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
  :after nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
  (circadian-themes . '((:sunrise . doom-monokai-machine)
                        (:sunset . doom-badger)
                        ("8:00" . tango-dark)
                        ("8:15" . zerodark)
                        ("15:00" . (solarized-gruvbox
                                   sixcolors
                                   sexy
                                   ember-twilight
                                   marron-gold
                                   doom-acario-dark
                                   doom-bluloco-dark
                                   doom-dark+
                                   doom-feather-dark
                                   doom-material-dark
                                   doom-solarized-dark-high-contrast
                                   doom-solarized-dark
                                   rg-themes-cappuccino-noir
                                   ;; rg-themes-ellas
                                   rg-themes-purpurina
                                   rg-themes-somnus
                                   ))
                        ;; ("8:45" . solarized-selenized-black)
                        ;; ("9:00" . solarized-selenized-dark)
                        ;; ("09:15" doom-winter-is-coming-dark-blue)
                        ;; doom-ayu-dark
                        ("15:15" . doom-bluloco-dark)
                        ("15:45" . doom-ephemeral)
                        ("16:00" . doom-plain-dark) ; should I end workday??
                        ("17:00" . sixcolors)
                        ("21:30" . doom-monokai-pro)))
  :config
  (circadian-setup)
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

(defcustom kb/terminal-theme 'wombat
  "Theme which should be activated when frame is open inside terminal."
  :type 'string
  :group 'kb-config)
;; (doom-one doom-snazzy)
;; (doom-moonlight doom-monokai-machine modus-vivendi-tinted misterioso)
(defcustom kb/window-theme 'doom-monokai-machine
  "Theme which should be activated when frame is open inside GUI."
  :type 'string
  :group 'kb-config)
(defvar kb/theme-window-loaded nil)
(defcustom kb/theme-window-font (if (eq system-type 'windows-nt)
                                    "Unifont"
                                        ;(set-frame-parameter nil 'font "Arial Unicode MS")
                                  "Hack"
                                  )
  "Font to used by my configuration.

The font defined here is used when switching Emacs frame between
GUI and TTY as well when changing between themes."
  :type 'face
  :group 'kb-config
  )
(defvar kb/theme-terminal-loaded nil)
(defvar kb/theme-original-font nil)

;; font configuration
(defun kb/set-window-font ()
  "Function set screen font.
If Emacs is run in MS Windows then use Arial Unicode MS
On U*x systems Use Hack"
  (setq kb/theme-original-font (frame-parameter nil 'font))
  (set-frame-parameter nil 'font kb/theme-window-font)
  ;; (set-frame-font kb/theme-window-font nil t)
  )

(defun kb/switch-font ()
  "Function set screen font.
Set original font."
  (interactive)
  (if (and kb/theme-original-font (eq kb/theme-original-font (frame-parameter nil 'font)))
      (kb/set-window-font)
    (set-frame-parameter nil 'font kb/theme-original-font)))

(define-key global-map (kbd "C-c o") 'kb/switch-font)
;;--------------------------------------------------------------------------------
(defun kb/load-graphics-theme ()
  "Function to load theme for GUI Emacs."
  (interactive)
  (unless kb/theme-window-loaded
    (setq kb/theme-window-loaded (load-theme kb/window-theme t t))
    (message "Theme `%S' loaded %S" kb/window-theme kb/theme-window-loaded))
  kb/theme-window-loaded)

(defun kb/load-terminal-theme ()
  "Function to load theme for TTY Emacs."
  (interactive)
  (unless kb/theme-terminal-loaded
    (setq kb/theme-terminal-loaded (load-theme kb/terminal-theme t t))
    (message "Theme `%S' loaded %S" kb/terminal-theme kb/theme-terminal-loaded))
  kb/theme-terminal-loaded)

(defun kb/activate-frame-theme (frame)
  "Activate theme depending on current FRAME window system.

If theme is'n loaded then it will be loaded at first"
  (interactive)
  (message "Activate frame theme")
  (select-frame frame)
  (cond ((window-system frame)
         (message (format "Activate frame %s graphical-theme" frame))
         (when (kb/load-graphics-theme)
           (message "Activate frame theme: load-graphics-theme")
           (enable-theme 'kb/window-theme))
         (message "Activate frame theme: set-window-font")
         (kb/set-window-font))
        ((kb/load-terminal-theme)
         (message (format "Activate frame %s theme: terminal-theme" frame))
         (enable-theme 'kb/terminal-theme)))
  )

(defun kb/activate-theme (&optional frame)
  "Set theme on active FRAME."
  (interactive)
  (message "Activate theme")
  (let ((frame (or frame (selected-frame))))
    (when (frame-focus-state frame)
      (message (format "Activate theme for frame: %s" frame))
      (kb/activate-frame-theme frame))
    ))

;; (kb/activate-theme)
;; (add-function :after after-focus-change-function #'kb/activate-theme)
;; (add-hook 'after-make-frame-functions-hook 'kb/load-frame-theme)

;; (frame-focus-state)
;; (              doom-modeline-update-env)

(leaf custom
  :doc "tools for declaring and initializing options"
  :tag "builtin" "faces" "help"
  :added "2022-11-04"
  :custom
  (custom-safe-themes . t)
  ;; (custom-enabled-themes . '(doom-monokai-machine wombat tango-dark))
  ;; (custom-enabled-themes . '(doom-monokai-machine))
  ;; :config
  ;; (message "custom: load kb/window-theme")
  ;; (load-theme kb/window-theme t)
  ;; (kb/activate-theme)
  ;; (message "custom: set kb/set-window-font")
  ;; (kb/set-window-font)
  )

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
(add-hook 'c-mode-hook 'kb/c-mode-hook)

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
  (push '("<=" . ?⩽) prettify-symbols-alist)
  (push '(">=" . ?⩾) prettify-symbols-alist)
  (push '("->" . ?→) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)
  )

(defun kb/c++-mode-hook ()
  "My style used while editing C++ sources."
  (c-add-style "kb/c++-style" kb/c++-style t)
  (auto-fill-mode)
  (display-fill-column-indicator-mode)
  (kb/c++-setup-symbol-compose)
  )
(add-hook 'c++-mode-hook 'kb/c++-mode-hook)

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
(leaf hl-todo
  :doc "Highlight TODO and similar keywords"
  :req "emacs-25.1" "compat-28.1.1.0"
  :tag "convenience" "emacs>=25.1"
  :url "https://github.com/tarsius/hl-todo"
  :added "2022-11-02"
  :emacs>= 25.1
  :ensure t
  :bind ((:hl-todo-mode-map
          ("C-c t p" . #'hl-todo-previous)
          ("C-c t n" . #'hl-todo-next)
          ("C-c t o" . #'hl-todo-occur)
          ("C-c t i" . #'hl-todo-insert)))
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

;; (defun kb/checkpatch-enable ()
;;   "Add checkpatch for diff files if project provides checkpatch.pl script."
;;   (if (file-exists-p "./scripts/checkpatch.pl")
;;       (progn (print "setting compile-command")
;;              (set (make-local-variable 'compile-command)
;;                   (concat "./scripts/checkpatch.pl --emacs "
;;                           (buffer-file-name))))
;;     (print "checkpatch not found")))
;; (add-hook 'diff-mode-hook 'kb/checkpatch-enable)

;; Highlight uncommitted changes
(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :req "cl-lib-0.2" "emacs-26.1"
  :tag "diff" "vc" "emacs>=26.1"
  :url "https://github.com/dgutov/diff-hl"
  :added "2025-03-27"
  :emacs>= 26.1
  :ensure t
  :hook (((prog-mode-hook vc-dir-mode-hook) . turn-on-diff-hl-mode)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

;; vc-hooks
(leaf vc
  :doc "drive a version-control system from within Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((vc-follow-symlinks . t)
           (vc-handled-backends . '(git svn))))

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

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "tools" "processes" "languages" "docker" "emacs>=24"
  :url "https://github.com/spotify/dockerfile-mode"
  :added "2025-01-25"
  :emacs>= 24
  :ensure t
  :config
  (pcase kb/docker-executable
    ('docker
     (setq dockerfile-mode-command "docker"))
    ('podman
     (setq dockerfile-docker-command "podman"))))

;; (leaf lsp-docker
;;   :doc "LSP Docker integration"
;;   :req "emacs-27.1" "dash-2.14.1" "lsp-mode-6.2.1" "f-0.20.0" "s-1.13.0" "yaml-0.2.0" "ht-2.0"
;;   :tag "langserver" "languages" "emacs>=27.1"
;;   :url "https://github.com/emacs-lsp/lsp-docker"
;;   :added "2025-01-20"
;;   :emacs>= 27.1
;;   :ensure t
;;   :after lsp-mode yaml
;;   :config
;;   (defvar lsp-docker-client-packages
;;     '(lsp-css lsp-clients lsp-bash lsp-go lsp-pylsp lsp-html lsp-typescript
;;               lsp-terraform lsp-clangd))

;;   (setq lsp-docker-client-configs
;;     '((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
;;       (:server-id clangd :docker-server-id clangd-docker :server-command "clangd")
;;       (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
;;       (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
;;       (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
;;       (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")
;;       (:server-id pylsp :docker-server-id pyls-docker :server-command "pylsp")
;;       (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")))

;;   (require 'lsp-docker)
;;   (lsp-docker-init-clients
;;    :path-mappings '(("~/projects/ascgit154.speechcore22/" . "~/projects/ascgit154.speechcore22/"))
;;    :client-packages lsp-docker-client-packages
;;    :client-configs lsp-docker-client-configs)
;;   )

(leaf posframe
  :doc "Pop a posframe (just a frame) at point"
  :req "emacs-26.1"
  :tag "tooltip" "convenience" "emacs>=26.1"
  :url "https://github.com/tumashu/posframe"
  :added "2025-01-20"
  :emacs>= 26.1
  :ensure t
  :require t)

(leaf stillness-mode
  :doc "Prevent windows from jumping on minibuffer activation"
  :req "emacs-26.1" "dash-2.18.0"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/neeasade/stillness-mode.el"
  :added "2025-02-03"
  :emacs>= 26.1
  :ensure t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
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
(leaf pretty-hydra
  :doc "A macro for creating nice-looking hydras"
  :req "hydra-0.15.0" "s-1.12.0" "dash-2.18.0" "emacs-24" "compat-29.1.4.1"
  :tag "emacs>=24"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :added "2025-03-11"
  :emacs>= 24
  :ensure t
  :after hydra compat)
(leaf major-mode-hydra
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
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(leaf flycheck-clang-tidy
  :doc "Flycheck syntax checker using clang-tidy"
  :req "flycheck-0.30"
  :tag "tools" "languages" "convenience"
  :url "https://github.com/ch1bo/flycheck-clang-tidy"
  :added "2025-05-26"
  :ensure t
  :after flycheck projectile
  :hook (flycheck-mode-hook . flycheck-clang-tidy-setup))
(leaf flycheck-google-cpplint
  :disabled t
  :after flycheck
  :defun flycheck-add-next-checker
  :require flycheck-google-cpplint
  :config
  (flycheck-add-next-checker 'c/c++-cppcheck
                             'c/c++-googlelint 'append))
(leaf flycheck-projectile
  :doc "Project-wide errors"
  :req "emacs-25.1" "flycheck-31" "projectile-2.2"
  :tag "emacs>=25.1"
  :url "https://github.com/nbfalcon/flycheck-projectile"
  :added "2025-02-03"
  :emacs>= 25.1
  :after flycheck projectile
  :ensure t)
(leaf flycheck-posframe
  :doc "Show flycheck error messages using posframe.el"
  :req "flycheck-0.24" "emacs-26" "posframe-0.7.0"
  :tag "emacs>=26"
  :url "https://github.com/alexmurray/flycheck-posframe"
  :added "2025-02-03"
  :emacs>= 26
  :ensure t
  :after flycheck posframe
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
  :after flycheck
  :advice ((:after ispell-pdict-save flycheck-maybe-recheck))
  :config
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
  )

(leaf unicode-fonts
  :doc "Configure Unicode fonts"
  :req "font-utils-0.7.8" "ucs-utils-0.8.2" "list-utils-0.4.2" "persistent-soft-0.8.10" "pcache-0.3.1"
  :tag "interface" "wp" "frames" "faces" "i18n"
  :url "http://github.com/rolandwalker/unicode-fonts"
  :added "2025-03-27"
  :ensure t
  :after font-utils ucs-utils list-utils persistent-soft pcache

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
  :require t
  :config
  (afterglow-mode t)
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
  )

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
  (require 'ox-md)
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

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-<print>") #'ews-org-insert-screenshot))
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
  :after org)

(leaf htmlize
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

(leaf ox-epub
  :doc "Export org mode projects to EPUB"
  :req "emacs-24.3" "org-9"
  :tag "hypermedia" "emacs>=24.3"
  :url "http://github.com/ofosos/org-epub"
  :added "2024-08-05"
  :emacs>= 24.3
  :ensure t
  :after org)

(leaf ox-gfm
  :doc "Github Flavored Markdown Back-End for Org Export Engine"
  :tag "github" "markdown" "wp" "org"
  :added "2023-04-13"
  :ensure t
  :after org
  :require ox-gfm)

(leaf ox-reveal
  :doc "reveal.js Presentation Back-End for Org Export Engine"
  :req "org-8.3"
  :tag "presentation" "slideshow" "hypermedia" "outlines"
  :added "2022-10-31"
  :ensure t
  :after org
  :custom (
           (org-reveal-root . "https://cdn.jsdelivr.net/npm/reveal.js")
           (org-reveal-mathjax . t))
  )

(leaf ox-jira
  :doc "JIRA Backend for Org Export Engine"
  :req "org-8.3"
  :tag "wp" "hypermedia" "outlines"
  :url "https://github.com/stig/ox-jira.el"
  :added "2022-10-31"
  :ensure t
  :after org
  :require ox-jira)
(leaf ox-mediawiki
  :doc "Mediawiki Back-End for Org Export Engine"
  :req "cl-lib-0.5" "s-1.9.0"
  :tag "mediawiki" "wp" "org"
  :url "https://github.com/tomalexander/orgmode-mediawiki"
  :added "2022-10-31"
  :ensure t
  :require ox-mediawiki
  :after org)
(leaf ox-tiddly
  :doc "Org TiddlyWiki exporter"
  :req "org-8" "emacs-24.4"
  :tag "org" "emacs>=24.4"
  :url "https://github.com/dfeich/org8-wikiexporters"
  :added "2022-10-31"
  ;; :emacs>= 24.4
  :ensure t
  :after org)
(leaf ox-trac
  :doc "Org Export Backend to Trac WikiFormat"
  :req "org-9.0"
  :tag "trac" "org-mode"
  :url "https://github.com/JalapenoGremlin/ox-trac"
  :added "2022-10-31"
  :ensure t
  :after org)
(leaf ox-twiki
  :doc "Org Twiki and Foswiki export"
  :req "org-8" "emacs-24.4"
  :tag "org" "emacs>=24.4"
  :url "https://github.com/dfeich/org8-wikiexporters"
  :added "2022-10-31"
  :after org)
(leaf ox-wk
  :doc "Wiki Back-End for Org Export Engine"
  :req "emacs-24.4" "org-8.3"
  :tag "wiki" "wp" "org" "emacs>=24.4"
  :url "https://github.com/w-vi/ox-wk.el"
  :added "2022-10-31"
  :ensure t
  :after org)

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
  :bind-keymap (:org-mode-map ("C-c C-0" . verb-command-map))
  :config
  (add-to-list 'org-babel-load-languages '((verb . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; (define-key org-mode-map (kbd "C-c C-0") verb-command-map)
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
         ("C-c C-e" . markdown-do)))
(leaf highlight-doxygen
  :doc "Highlight Doxygen comments"
  :tag "faces"
  :url "https://github.com/Lindydancer/highlight-doxygen"
  :added "2022-10-31"
  :ensure t
  :hook c-mode-common-hook)

;; (leaf pabbrev
;;   :doc "Predictive abbreviation expansion"
;;   :added "2022-10-31"
;;   :ensure t
;;   :config (global-pabbrev-mode))

(leaf abbrev
  ;; :blackout abbrev-mode
  :custom
  (save-abbrevs . t)
  :config
  (setq-default abbrev-mode t)
  )

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
  :ensure t
  :hook ((c-mode-common-hook text-mode-hook fundamental-mode-hook) . ws-butler-mode))

(leaf cycle-quotes
  :ensure t
  :bind ("C-c q" . cycle-quotes))

(leaf bool-flip
  :ensure t
  :bind ("C-c C-b" . bool-flip-do-flip))

(message "clang")
(when (not (executable-find "clang")) (message "clang executable not found"))

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
  :bind (("C-M-'" . clang-format-region)))
(leaf clang-format+
  :doc "Minor mode for automatic clang-format application"
  :req "emacs-25.1" "clang-format-20180406.1514"
  :tag "clang-format" "c++" "c" "emacs>=25.1"
  :url "https://github.com/SavchenkoValeriy/emacs-clang-format-plus"
  :added "2022-10-31"
  :emacs>= 25.1
  :after clang-format
  :ensure t
  :custom (clang-format+-context 'modification)
  ;;:hook (c-mode-common-hook . clang-format+-mode)
  )

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  ;; :after spinner markdown-mode lv
  ;; :commands (lsp lsp-deferred which-key)
  ;; :init
  ;; (customize-set-variable 'lsp-keymap-prefix "C-c l" "Configured by Init for lsp-mode") ;; Or 'C-l', 's-l'
  :custom
  ;; (lsp-print-performance t)
  ((lsp-enable-xref . t)
   (lsp-log-io . nil)
   (lsp-idle-delay . 0.5)
   (lsp-keymap-prefix . "C-c l")
   (lsp-completion-provider . :none)
   (lsp-headerline-breadcrumb-enable . t)
   ;; (lsp-headerline-breadcrumb-segments . '(symbols project))
   (lsp-enable-snippet . nil)
   ;; Super super slow to have LSP on remote files because TRAMP is not
   ;; async.
   (lsp-auto-register-remote-clients . nil)
  )
  :hook ((c-mode-common-hook . lsp-deferred)
         (cmake-mode-hook . lsp-deferred))
  :config
  (defun kb/lsp-breadcrumb-face-setup ()
    "Fix headerlime colors for breadcrumbs"
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil :foreground "yellow" :background 'unspecified :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil  :foreground "PaleGreen" :background 'unspecified :width 'extra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-separator-face nil :foreground "green" :background 'unspecified :weight 'ultra-bold :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil :foreground "green" :background 'unspecified :weight 'light :width 'ultra-condensed)
    (set-face-background 'header-line "black")
    )
  :hook
  (lsp-headerline-breadcrumb-mode-hook . kb/lsp-breadcrumb-face-setup)

  :config
  (leaf lsp-mode-which-key
    :doc "Enable which-key integration after which-key and lsp-mode are loaded"
    :after which-key lsp-mode
    :config (lsp-enable-which-key-integration t))

  (leaf lsp-yasnippet
    :after yasnippet lsp-mode
    :custom (lsp-enable-snippet . t))
  )
(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-27.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=27.1"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :added "2025-02-03"
  :emacs>= 27.1
  :after lsp-mode markdown-mode
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:lsp-ui-mode-map
         ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode
  :custom (
           (lsp-ui-doc-delay . 1)
           (lsp-ui-doc-enable . t)
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-show-with-cursor . t)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-position . 'bottom)
           (lsp-ui-imenu-enable . t)
           (lsp-ui-peek-enable . t)
           (lsp-ui-sideline-enable . nil)  ; disable lsp sideline as I'm using sideline package
           (lsp-ui-flycheck-list-position . 'right)
           (lsp-ui-sideline-show-code-actions . t)
           (lsp-ui-sideline-show-diagnostics . t)))

(leaf sideline
  :doc "Show information on the side"
  :req "emacs-28.1" "ht-2.4"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline"
  :added "2025-05-15"
  :emacs>= 28.1
  :ensure t
  :hook ((flycheck-mode-hook lsp-mode-hook) . sideline-mode))
(leaf sideline-flycheck
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
(leaf sideline-lsp
  :doc "Show lsp information with sideline"
  :req "emacs-28.1" "sideline-0.1.0" "lsp-mode-6.0" "dash-2.18.0" "ht-2.4" "s-1.12.0"
  :tag "lsp" "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline-lsp"
  :added "2025-05-15"
  :emacs>= 28.1
  :ensure t
  :after sideline lsp-mode
  :config
  (add-to-list 'sideline-backends-right 'sideline-lsp))

(leaf cmake-mode
  :doc "Major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2025-01-17"
  :emacs>= 24.1
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook ((cmake-mode-hook . (lambda ()
		              (message "CmakeMode custom")
		              (setq fill-column 80)
		              (auto-fill-mode)
		              (setq cmake-tab-width 4)
		              (setq indent-tabs-mode nil)))))

(leaf cmake-font-lock
  :doc "Advanced, type aware, highlight support for CMake"
  :req "cmake-mode-0.0"
  :tag "languages" "faces"
  :url "https://github.com/Lindydancer/cmake-font-lock"
  :added "2025-01-17"
  :ensure t
  :after cmake-mode)

(leaf treemacs
  :doc "A tree style file explorer package"
  :req "emacs-26.1" "cl-lib-0.5" "dash-2.11.0" "s-1.12.0" "ace-window-0.9.0" "pfuture-1.7" "hydra-0.13.2" "ht-2.2" "cfrs-1.3.2"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2025-01-17"
  :emacs>= 26.1
  :after ace-window pfuture hydra cfrs
  :ensure t
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
  :after edit-indirect
  :custom
  (separedit-default-mode . 'markdown-mode)
  (separedit-preserve-string-indentation . t)
  (separedit-continue-fill-column . t)
  (separedit-write-file-when-execute-save . t)
  (separedit-remove-trailing-spaces-in-comment . t)
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
  :config
  (message "spareedit configured")
  (require 'separedit))

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
           (dirvish-attributes . '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
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
  (("C-c f" . dirvish-fd)
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

(leaf lsp-treemacs
  :doc "LSP treemacs"
  :req "emacs-27.1" "dash-2.18.0" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
  :tag "languages" "emacs>=27.1"
  :url "https://github.com/emacs-lsp/lsp-treemacs"
  :added "2025-01-17"
  :emacs>= 27.1
  :ensure t
  :after treemacs lsp-mode
  :commands lsp-treemacs-errors-list)

(leaf dap-mode
  :disabled t
  :doc "Debug Adapter Protocol mode"
  :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "bui-1.1.0" "f-0.20.0" "s-1.12.0" "lsp-treemacs-0.1" "posframe-0.7.0" "ht-2.3" "lsp-docker-1.0.0"
  :tag "debug" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/dap-mode"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode
  :require 'dap-cpptools
  )

;; (leaf ztree
;;   :doc "Text mode directory tree"
;;   :req "cl-lib-0"
;;   :tag "tools" "files"
;;   :url "https://github.com/fourier/ztree"
;;   :added "2022-10-31"
;;   :ensure t)

(leaf which-key
  :doc "Display available keybindings in popup"
  :tag "builtin"
  :added "2025-01-16"
  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  :custom ((which-key-idle-delay . 1)
           (which-key-show-early-on-C-h . t)
           (which-key-idle-delay . most-positive-fixnum)
           (which-key-idle-secondary-delay . 1e-100))
  :config
  (add-to-list 'which-key-replacement-alist
               '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))

  (add-to-list 'which-key-replacement-alist
               '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))

  (which-key-mode)
  :config
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
    :after posframe which-key
    :ensure t
    :custom (which-key-posframe-font . "Liberation Mono")
    :config (which-key-posframe-mode)))
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
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face . '((t (:foreground "#cba6f7"))))
  )
(leaf yaml-imenu
  :doc "Enhancement of the imenu support in yaml-mode"
  :req "emacs-24.4" "yaml-mode-0"
  :tag "imenu" "convenience" "outlining" "emacs>=24.4"
  :url "https://github.com/knu/yaml-imenu.el"
  :added "2025-02-05"
  :emacs>= 24.4
  :ensure t
  :after yaml-mode)
(leaf yaml
  :doc "YAML parser for Elisp"
  :req "emacs-25.1"
  :tag "tools" "emacs>=25.1"
  :url "https://github.com/zkry/yaml.el"
  :added "2025-02-05"
  :emacs>= 25.1
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
  :doc "major mode for editing protocol buffers."
  :tag "languages" "protobuf" "google"
  :added "2022-10-31"
  :disabled t
  :config
  (defconst kb/protobuf-style
    '("linux"
      (c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode-hook . (lambda () (c-add-style "kb/protobuf" kb/protobuf-style t)))
  )

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3"
  :tag "lisp" "convenient" "emacs>=24.3"
  :url "https://github.com/domtronn/all-the-icons.el"
  :added "2022-10-31"
  :emacs>= 24.3
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t))
  (leaf all-the-icons-ibuffer
    :doc "Display icons for all buffers in ibuffer"
    :req "emacs-24.4" "all-the-icons-2.2.0"
    :tag "ibuffer" "icons" "convenience" "emacs>=24.4"
    :url "https://github.com/seagle0128/all-the-icons-ibuffer"
    :added "2022-10-31"
    :emacs>= 24.4
    :ensure t
    :hook ibuffer-mode-hook)
  (leaf treemacs-all-the-icons
    :doc "all-the-icons integration for treemacs"
    :req "emacs-26.1" "all-the-icons-4.0.1" "treemacs-0.0"
    :tag "emacs>=26.1"
    :url "https://github.com/Alexander-Miller/treemacs"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t)
  (leaf all-the-icons-completion
    :doc "Add icons to completion candidates"
    :req "emacs-26.1" "all-the-icons-5.0"
    :tag "lisp" "convenient" "emacs>=26.1"
    :url "https://github.com/iyefrat/all-the-icons-completion"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
    :init (all-the-icons-completion-mode))
  )
(leaf treemacs-icons-dired
  :doc "Treemacs icons for dired"
  :req "treemacs-0.0" "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :after treemacs)

(leaf hydra
  :doc "Make bindings that stick around."
  :req "cl-lib-0.5" "lv-0"
  :tag "bindings"
  :url "https://github.com/abo-abo/hydra"
  :added "2022-10-31"
  :ensure t
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
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
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
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
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
  :hook (((completion-list-mode-hook embark-collect-mode-hook) . consult-preview-at-point-mode))

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
  :bind (("C-c p p" . #'consult-projectile)
         ([remap projectile-switch-project] . #'consult-projectile)
         ([remap projectile-recentf] . #'consult-projectile-recentf)
         ([remap projectile-find-file] . #'consult-projectile-find-file)
         ([remap projectile-find-dir] . #'consult-projectile-find-dir)
         ([remap projectile-switch-to-buffer] . #'consult-projectile-switch-to-buffer)
         )
  :config
  (setopt consult-projectile-sources
          (append '(consult-projectile--source-projectile-dir consult-projectile--source-projectile-recentf)
                   consult-projectile-sources)))
(leaf consult-flycheck
  :doc "Provides the command `consult-flycheck'"
  :req "emacs-28.1" "consult-1.8" "flycheck-34"
  :tag "completion" "tools" "languages" "emacs>=28.1"
  :url "https://github.com/minad/consult-flycheck"
  :added "2025-06-06"
  :emacs>= 28.1
  :ensure t
  :after consult flycheck)
(leaf consult-company
  :disabled t
  :doc "Consult frontend for company"
  :req "emacs-27.1" "company-0.9" "consult-0.9"
  :tag "emacs>=27.1"
  :url "https://github.com/mohkale/consult-company"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after company consult)
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
				           ("C-x C-j" . #'consult-dir-jump-file))
         )
  :hook
  ((projectile-mode-hook . (lambda (&rest _) (message "Run consult-dir hook") (setq consult-dir-project-list-function 'consult-dir-projectile-dirs)))))
(leaf consult-lsp
  :doc "LSP-mode Consult integration"
  :req "emacs-27.1" "lsp-mode-5.0" "consult-1.9" "f-0.20.0"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/gagbo/consult-lsp"
  :added "2025-06-06"
  :emacs>= 27.1
  :ensure t
  :after lsp-mode consult
  :bind
  (:lsp-mode-map ([remap xref-find-apropos] . #'consult-lsp-symbols)))

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

(leaf vertico
  :doc "VERTical Interactive COmpletion."
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "files" "convenience" "emacs>=28.1"
  :url "https://github.com/minad/vertico"
  :added "2025-06-10"
  :emacs>= 28.1
  :ensure t
  :after compat
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
  :hook after-init-hook
  :custom ((vertico-cycle . t)
           (vertico-resize . t)
           (vertico-scroll-margin . 0)
           (vertico-preselect . 'first))


  ;; do not consider case significant in completion (GNU Emacs default)
  (setq completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  :config
  (message "Configure vertico")
  (vertico-mode)
  (vertico-mouse-mode +1)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
#'consult-completion-in-region
	           #'completion--in-region)
	         args)))

  ;; Don't re-sort buffer candidates. The recency order is correct.
  ;; (vertico-multiform-mode +1)
  (setq vertico-multiform-categories
        '((buffer (vertico-sort-function . copy-sequence))))
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
  :after posframe vertico
  :config
  (vertico-posframe-mode))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/oantolin/orderless"
  :added "2022-11-24"
  :emacs>= 26.1
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)

  ;; (setq completion-styles '(basic substring partial-completion flex))
  (completion-styles . '(substring orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion))))
  ;; (orderless-component-separator . " +\\|[-/]")
  (orderless-component-separator . "[ &]")

  :config
  ;; (defun just-one-face (fn &rest args)
  ;;   (let ((orderless-match-faces [completions-common-part]))
  ;;     (apply fn args)))

  ;; (advice-add 'company-capf--candidates :around #'just-one-face)

  ;; We follow a suggestion by company maintainer u/hvis:
  ;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
  ;; (defun company-completion-styles (capf-fn &rest args)
  ;;   (let ((completion-styles '(basic partial-completion)))
  ;;     (apply capf-fn args)))

  ;;   (advice-add 'company-capf :around #'company-completion-styles)
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
  :init
  ;; Must be in the :init section of use-package/leaf such that the
  ;; mode gets enabled right away. Note that this forces loading the
  ;; package
  (marginalia-mode +1))

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
  :require t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("M-." . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
         ("<f2> i" . #'embark-info-lookup-symbol)
         ("<f2> u" . #'embark-save-unicode-character))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command . #'embark-prefix-help-command)

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
	         (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-28.1" "compat-30" "embark-1.1" "consult-1.8"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-06-06"
  :emacs>= 28.1
  :after compat embark consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

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
  (completion-preview-sort-function . #'prescient-completion-sort)
  :after prescient
  :config
  (message "Enable prescient-persist-mode")
  (prescient-persist-mode)
  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000)
  ;; Common sense.
  (setq prescient-sort-full-matches-first t))

(leaf vertico-prescient
  :ensure t
  :after vertico
  :config
  (message "Configure vertico-perscient")
  (vertico-prescient-mode))
(leaf corfu-prescient
  :ensure t
  :after corfu
  :config
  (message "Configure corfu-perscient")
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
  (corfu-popupinfo-delay . '(0.5 . 0.2))
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :hook (eshell-mode-hook . (lambda () (setq-local corfu-quit-at-boundary t
                                              corfu-quit-no-match t
                                              corfu-auto nil)
                              (corfu-mode)))
  :bind (:corfu-map
         ("M-SPC" . corfu-insert-separator)
         ("RET" . nil) ; Leave my enter alone!
         ("TAB" . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("M-RET" . corfu-insert)
         ("S-RET" . corfu-insert)
         ("C-RET" . corfu-insert))
  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode -1))

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
  :disabled t
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

  :config
  (message "Configure cape")
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)

  ;; (add-to-list 'completion-at-point-functions #'cape-file t)
  ;; ;; Nice completion to have available everywhere
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  ;; (add-to-list 'completion-at-point-functions #'cape-emoji t)

  (when (< emacs-major-version 29)
    ;; Silence then pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; Ensure that pcomplete does not write to the buffer and behaves as
    ;; pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  ;; (defalias 'cape-dabbrev-min-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  ;; (setq-local completion-at-point-functions (list (cape-capf-prefix-length #'cape-dabbrev 2)))
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
  ;; According to the POSIX, a line is defined as "a sequence of zero or
  ;; more non-newline characters followed by a terminating newline".
  (require-final-newline . t)
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

(leaf windower
  :disabled t
  :ensure t
  :bind (("<s-M-left>" . windower-move-border-left)
         ("<s-M-right>" . windower-move-border-right)
         ("<s-M-down>" . windower-move-border-below)
         ("<s-M-up>" . windower-move-border-above)
         ("<s-M-tab>" . windower-switch-to-last-buffer)
         ("<s-S-o>" . windower-toggle-single)
         ("s-\\" . windower-toggle-split)
         ))

(unless (executable-find "cargo") (warn "Cargo Rust package manager not found"))
(leaf fuz
  ;; not working with emacs from ubuntu/snap
  :disabled t
  :doc "Fast and precise fuzzy scoring/matching utils"
  :req "emacs-25.1"
  :tag "lisp" "emacs>=25.1"
  :url "https://github.com/cireu/fuz.el"
  :added "2025-01-17"
  :emacs>= 25.1
  :ensure t
  :when (executable-find "cargo")
  :require fuz
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  )

(leaf smart-compile
  :doc "An interface to `compile'"
  :tag "unix" "tools"
  :url "https://github.com/zenitani/elisp"
  :added "2025-01-17"
  :ensure t
  :require smart-compile)

(leaf zygospore
  :doc "Reversible C-x 1 (delete-other-windows)"
  :url "https://github.com/louiskottmann/zygospore.el"
  :added "2025-01-17"
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

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

(defun kb/keymap-prefix (keymap)
  "Print KEYMAP prefixes in *Messages* buffer."
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
  ;; :require t
  ;; :blackout t ; (projectile-project-name);  '(:eval (concat " " (projectile-project-name)))
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
    (message "Magit known projects added"))

  (message "Configure projectile mode")
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

(leaf lsp-python-ms
  :disabled t
  :doc "The lsp-mode client for Microsoft python-language-server"
  :req "emacs-25.1" "lsp-mode-6.1"
  :tag "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-lsp/lsp-python-ms"
  :added "2022-10-31"
  :emacs>= 25.1
  :after lsp-mode python
  :hook (python-mode-hook . (lambda ()
		              (require 'lsp-python-ms)
		              (lsp)))
  :custom ((lsp-python-ms-auto-install-server . t))
  )

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :added "2023-06-21"
  :emacs>= 25.1
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'lsp)
  )

(leaf flycheck-rust
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
         ("C-x M-g" . magit-dispatch))
  :config
  (put 'magit-clean 'disabled nil)
  (leaf treemacs-magit
    :doc "Magit integration for treemacs"
    :req "emacs-26.1" "treemacs-0.0" "pfuture-1.3" "magit-2.90.0"
    :tag "emacs>=26.1"
    :url "https://github.com/Alexander-Miller/treemacs"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :after treemacs pfuture magit)
  )

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

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

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
  :after tsc
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  )
(leaf tree-sitter-langs
  :doc "Grammar bundle for tree-sitter"
  :req "emacs-25.1" "tree-sitter-0.15.0"
  :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/tree-sitter-langs"
  :added "2025-01-27"
  :emacs>= 25.1
  :ensure t
  :after tree-sitter
  :require t)
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

(leaf git-timemachine
  :doc "Walk through git revisions of a file"
  :req "emacs-24.3" "transient-0.1.0"
  :tag "vc" "emacs>=24.3"
  :url "https://gitlab.com/pidu/git-timemachine"
  :added "2022-10-31"
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

(leaf d2-mode
  :disabled t
  :doc "Major mode for working with d2 graphs"
  :req "emacs-26.1"
  :tag "processes" "tools" "graphs" "d2" "emacs>=26.1"
  :url "https://github.com/andorsk/d2-mode"
  :added "2022-12-13"
  :when (executable-find "d2")
  :emacs>= 26.1
  :ensure t
  :mode ("\\.d2\\'")

  ;; ================ TALA rendering engine installation ===============
  ;; With --dry-run the install script will print the commands it will use
  ;; to install without actually installing so you know what it's going to do.
  ;; curl -fsSL https://d2lang.com/install.sh | sh -s -- --tala --dry-run
  ;; If things look good, install for real.
  )

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
;;  	            ("d" yas-load-directory)
;;  	            ("e" yas-activate-extra-mode)
;;  	            ("i" yas-insert-snippet)
;;  	            ("f" yas-visit-snippet-file :color blue)
;;  	            ("n" yas-new-snippet)
;;  	            ("t" yas-tryout-snippet)
;;  	            ("l" yas-describe-tables)
;;  	            ("g" yas/global-mode)
;;  	            ("m" yas/minor-mode)
;;  	            ("a" yas-reload-all))
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
           (org-plantuml-jar-path . kb/plantuml-jar-path)
           )
  :hook (plantuml-mode-hook . (lambda ()
			        (set-fill-column 100)
			        (display-fill-column-indicator-mode)
			        ))
  :bind (:plantuml-mode-map
         ("C-c C-p" . plantuml-preview-buffer))
  :config
  (plantuml-set-output-type "svg")
  (add-to-list 'org-babel-load-languages '((plantuml . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (if (not (file-exists-p kb/plantuml-jar-path))
      (warn (format "PlantUML JAR (%s) not found. Download by running '(plantuml-download-jar) function." kb/plantuml-jar-path))
    )
  :config
  (leaf flycheck-plantuml
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

(leaf json-mode
  :doc "Major mode for editing JSON files."
  :req "json-snatcher-1.0.0" "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/joshwnj/json-mode"
  :added "2022-11-01"
  :emacs>= 24.4
  :ensure t
  :after json-snatcher
  :config
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
    ((js-mode-hook js2-mode-hook) . js-mode-bindings)
    ))

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
  :disabled t
  :ensure t
  :doc "a new XML mode"
  :tag "builtin" "xml" "languages" "hypermedia" "wp"
  :added "2023-08-30"
  :config
  (defun xml-find-file-hook ()
    (when (derived-mode-p 'nxml-mode)
      (which-function-mode t)
      (setq which-func-mode t)
      (add-hook 'which-func-functions 'nxml-where t t)))

  (add-hook 'find-file-hook 'xml-find-file-hook t))

(leaf go-translate
  :doc "Translation framework, configurable and scalable"
  :req "emacs-28.1"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/lorniu/go-translate"
  :added "2024-05-21"
  :emacs>= 28.1
  :ensure t
  :custom
  (gt-langs .'(en de ja ko pt pl fr zh))
  :bind
  (("<f5>" . #'gt-do-translate))
  ;; :config
  ;; (gt-taker :text 'buffer :langs '(en pl) :prompt 'buffer :pick 'sentence)
  ;; (gt-taker :text 'buffer :langs '(zh en) :prompt 'buffer :pick 'sentence)
  ;; (setq gt-default-translator (gt-translator
  ;;                              :taker (gt-taker)
  ;;                              :engines (list (gt-bing-engine)
  ;;                                             (gt-google-engine :parse (gt-google-summary-parser))
  ;;                                             (gt-deepl-engine :pro nil))
  ;;                              :render (gt-buffer-render)
  ;;                               ;; (gts-posframe-pin-render)
  ;;                               ;; (gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
  ;;                               ;; (gts-kill-ring-render)
  ;;                               ;; :splitter
  ;;                               ;; nil
  ;;                               ;; (gt-paragraph-splitter)
  ;;                               ))
  )

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
         ;; ("<SPC>" . #'haskell-mode-contextual-space)
         )
  :custom ((haskell-stylish-on-save . t)
           (haskell-process-type . 'cabal-repl)
           (haskell-process-suggest-remove-import-lines . t)
           (haskell-process-auto-import-loaded-modules . t)
           (haskell-process-log . t)
           )
  :config
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

(leaf lsp-haskell
  :doc "Haskell support for lsp-mode"
  :req "emacs-24.3" "lsp-mode-3.0" "haskell-mode-16.1"
  :tag "haskell" "emacs>=24.3"
  :url "https://github.com/emacs-lsp/lsp-haskell"
  :added "2023-03-08"
  :emacs>= 24.3
  :ensure t
  :after lsp-mode haskell-mode
  :hook (((haskell-mode-hook haskell-literate-mode-hook) . lsp)
         (lsp-after-initialize-hook . (lambda () (lsp--set-configuration '(:haskell (:plugin (:tactics (:config (:timeout_duration 5))))))))))

;; (leaf copilot
;;   :doc "An unofficial Copilot plugin"
;;   :req "emacs-27.2" "s-1.12.0" "dash-2.19.1" "editorconfig-0.8.2" "jsonrpc-1.0.14" "f-0.20.0"
;;   :tag "copilot" "convenience" "emacs>=27.2"
;;   :url "https://github.com/copilot-emacs/copilot.el"
;;   :added "2025-01-27"
;;   :emacs>= 27.2
;;   :ensure t
;;   :after editorconfig jsonrpc)

(leaf copilot-chat
  :disabled t
  :doc "Copilot chat interface"
  :req "request-0.3.2" "markdown-mode-2.6" "emacs-27.1" "chatgpt-shell-1.6.1" "magit-4.0.0"
  :tag "tools" "convenience" "emacs>=27.1"
  :url "https://github.com/chep/copilot-chat.el"
  :added "2024-11-18"
  :emacs>= 27.1
  :ensure t
  :after markdown-mode chatgpt-shell magit
  :hook
  (copilot-chat-insert-commit-message . git-commit-setup-hook)
  ;; :custom
  ;; (copilot-chat-frontend . 'markdown)
  )

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

(message "Init finished")
;;; init.el ends here
