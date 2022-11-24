;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix -*-
;;; Commentary:
;; Load all configuration parts

;;; Code:
;; don't let Customize mess with my .emacs
(defconst kb/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun kb/emacs-subdirectory (dir)
  "Return DIR as subdirectory of HOME."
  (expand-file-name dir kb/emacs-directory))

(setq custom-file (expand-file-name "custom.el" kb/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;
(setq user-full-name "Karol Barski")

(defconst kb/environment-script (expand-file-name "environment.el" (kb/emacs-subdirectory "rc")))
(when (file-exists-p kb/environment-script)
  (load-file "~/.emacs.d/rc/environment.el"))

(add-to-list 'load-path (kb/emacs-subdirectory "rc"))
(add-to-list 'load-path (kb/emacs-subdirectory "site-lisp/org-addons"))

(when (file-exists-p (expand-file-name "rc-functions.el" (kb/emacs-subdirectory "rc")))
  (load "~/.emacs.d/rc/rc-functions.el"))

(setq read-process-output-max (* 4 1024 1024))
(setq tab-width 4)                       ; default to 4 visible spaces to display a tab
(setq indicate-empty-lines t)

;; MuLe commands
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; blink screen on bell
(setq visible-bell t)
;;(setq debug-on-error nil)

;; dim the ignored part of the file name
;;(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

;; do not consider case significant in completion (GNU Emacs default)
;; (setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq load-prefer-newer t)

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
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;;--------------------------------------------------------------------------------
;; Elpa setup
;;--------------------------------------------------------------------------------
(if (version< emacs-version "24")
    ;; install elpa on emacs 23
    (progn
      (when (load (expand-file-name "~/.emacs.d/package.el"))
	(package-initialize)))
  "Emacs >= 24 has elpa integrated")

(require 'cl-lib)
;; (package-initialize)
;; (setq package-check-signature nil)
;; (require 'package)

(defvar kb/package-archives ()
  "Set of achives which will be finally set to package-achives.")

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
  (add-to-list 'kb/package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'kb/package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'kb/package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
    )
  )

;;(add-to-list 'package-archives
;;           '("gnu" . "http://elpa.gnu.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (add-to-list 'kb/package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'kb/package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'kb/package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (setq package-archives kb/package-archives)

;; (defun kb/ensure-package-installed (&rest packages)
;;   "Assure every package is installed.
;; Return a list of installed packages or nil of every skipped package.

;; PACKAGES: list of packages to install."
;;   (mapcar
;;    (lambda (package)
;;      (if (package-installed-p package)
;; 	 package
;;        (package-install package)))
;;    packages))

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
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; (leaf async-await :el-get chuntaro/emacs-async-await)
;; (leaf promise :el-get chuntaro/emacs-promise)
;; (leaf iter2 :el-get doublep/iter2)
;; (leaf async :el-get jwiegley/emacs-async)
;; (leaf feather
;;   :doc "Parallel thread modern package manager"
;;   :req "emacs-26.3" "async-1.9" "async-await-1.0" "ppp-1.0" "page-break-lines-0.1"
;;   :tag "package" "convenience" "emacs>=26.3"
;;   :url "https://github.com/conao3/feather.el"
;;   :added "2022-11-01"
;;   :emacs>= 26.3
;;   :ensure t
;;   :after ppp page-break-lines
;;   :config (feather-mode 1)
;;   )
;; (leaf feather
;;   :el-get conao3/feather.el
;;   :after page-break-lines
;;   :config (feather-mode 1)
;;   ;; (leaf page-break-lines
;;   ;;   :doc "Display ^L page breaks as tidy horizontal lines"
;;   ;;   :req "emacs-24.4"
;;   ;;   :tag "faces" "convenience" "emacs>=24.4"
;;   ;;   :url "https://github.com/purcell/page-break-lines"
;;   ;;   :added "2022-11-01"
;;   ;;   :emacs>= 24.4
;;   ;;   :el-get t)
  
;;   )
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

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
  :init (defconst kb/frame-config '(;; (top . 1)
			            ;; (left . 1)
			            ;; (fullscreen . maximized)
			            (menu-bar-lines . 0)       ; turn menus off
			            (tool-bar-lines . 0)       ; disable toolbar
			            (scroll-bar-width . 10)
			            (vertical-scroll-bars . 'right)
			            ;; (background-mode . dark)
			            )
          )
  :setq ((initial-frame-alist . kb/frame-config)
         (default-frame-alist . kb/frame-config)
         )
  :custom ((blink-cursor-mode . nil)
	   ;; turn off blinking cursor
	   (blink-cursor-blinks . 3)
	   (blink-cursor-delay . 1)
           )
  :config
  ;; (blink-cursor-mode -1)
  ;; (mapc 'frame-set-background-mode (frame-list))
  ;; (fullscreen-restore . fullheight)
  ;; (fullscreen . fullboth)
  )

(leaf alloc
  :tag "builtin"
  :setq `((gc-cons-threshold . ,(* 512 1024 1024))
          (read-process-output-max . ,(* 4 1024 1024))
          (garbage-collection-messages . t)))

(leaf auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)


;;--------------------------------------------------------------------------------
;; My emacs config
;;--------------------------------------------------------------------------------

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :custom ((inhibit-startup-screen . t)
           (user-mail-address . "karol.barski@mobica.com")
           )
  )
(setq line-spacing 0)
;; (help-char "? M-?")
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-key help-map "?" 'describe-key-briefly)

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
           )
  )

(leaf indent
  :doc "indentation commands for Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((tab-always-indent . 'complete)))


;; Use M-/ for `company` completion
(define-key input-decode-map "\e[1;2A" [S-up])

(leaf page-break-lines
  :doc "Display ^L page breaks as tidy horizontal lines"
  :req "emacs-24.4"
  :tag "faces" "convenience" "emacs>=24.4"
  :url "https://github.com/purcell/page-break-lines"
  :added "2022-11-02"
  :emacs>= 24.4
  :ensure t
  :config (global-page-break-lines-mode))

(leaf vc
  :doc "drive a version-control system from within Emacs"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((vc-follow-symlinks . t))
  )
(leaf help
  :doc "help commands for Emacs"
  :tag "builtin" "internal" "help"
  :added "2022-11-01"
  :custom ((temp-buffer-resize-mode . t))
  )
;; image
(setq auto-image-file-mode 1)
;; time
;; (setq display-time-format "%H:%M %d/%m/%Y")
;; (setq display-time-24hr-format t)
;; (display-time)

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

(define-key global-map (kbd "C-a") 'prelude-move-beginning-of-line)
;;  (define-key global-map (kbd "C-c i") 'indent-region-or-buffer)
;; (define-key global-map (kbd "M-o") 'prelude-smart-open-line)
(define-key global-map (kbd "%") 'match-parenthesis) ;; % key on paren moves cursor to matching paren
(define-key global-map (kbd "C-c T") 'kb/delete-trailing-whitespaces-and-untabify)
(define-key global-map (kbd "C-c u") 'kb/set-buffer-eol-unix)
(define-key global-map (kbd "C-c d") 'kb/set-buffer-eol-dos)
;; (define-key global-map (kbd "C-c m") 'kb/set-buffer-eol-mac)
(define-key global-map (kbd "C-c C-d") 'kb/insert-date-time)

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

(defun kb/filename ()
  "Copy the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(define-key global-map (kbd "C-c f") 'kb/filename)

;; I know that string is in my Emacs somewhere!
;; (require 'cl)
(defcustom kb/search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

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
(leaf color-moccur
  :doc "multi-buffer occur (grep) mode"
  :tag "convenience"
  :url "http://www.bookshelf.jp/elc/color-moccur.el"
  :added "2022-11-01"
  :disabled t
  :ensure t
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
	 (:isearch-mode-map
	  ("M-o" . isearch-moccur)
	  ("M-O" . isearch-moccur-all)))
  :custom ((isearch-lazy-highlight . t))
  ;; :config
  ;; (leaf moccur-edit :ensure t)
  )

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
  :bind (("C-c w" . whitespace-mode))
  :custom-face
  (whitespace-space . '((t (:inherit whitespace-space :foreground "DimGrey" :background nil))))
  (whitespace-newline . '((t (:inherit whitespace-newline :foreground "DimGrey" :background nil))))
  (whitespace-indentation . '((t (:inherit whitespace-indentation :foreground "DimGrey" :background nil))))
 
  :config
  (global-whitespace-mode 1)
  )

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
  ;; (doom-themes-visual-bell-config)
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
  :ensure t
  :config
  (solaire-global-mode))

(leaf doom-modeline
  :ensure t
  :hook after-init-hook
  :custom (
           (doom-modeline-hud . t)
           (doom-modeline-buffer-file-name-style . 'truncate-with-project)
           ;; (setq doom-modeline-enable-word-count nil)
           (doom-modeline-buffer-encoding . 'nondefault)
           (doom-modeline-default-coding-system . 'utf-8)
           (doom-modeline-default-eol-type . 0)
           (doom-modeline-vcs-max-length . 24)))

(defvar kb/terminal-theme 'wombat)
;; (defvar kb/window-theme 'doom-one)
;; (defvar kb/window-theme 'doom-snazzy)
(defvar kb/window-theme 'doom-monokai-machine)
;; (defvar kb/window-theme 'doom-one)
;; (defvar kb/window-theme 'misterioso)
(defvar kb/theme-window-loaded nil)
(defvar kb/theme-window-font (if (eq system-type 'windows-nt)
				 "Unifont"
					;(set-frame-parameter nil 'font "Arial Unicode MS")
			       "Hack"
			       ))

(defvar kb/theme-terminal-loaded nil)
(defvar kb/theme-original-font nil)

;; font configuration
(defun kb/set-window-font ()
  "Function set screen font.
If Emacs is run in MS Windows then use Arial Unicode MS
On U*x systems Use Hack"
  (setq kb/theme-original-font (frame-parameter nil 'font))
  (set-frame-parameter nil 'font kb/theme-window-font))

(defun kb/switch-font ()
  "Function set screen font.
Set original font."
  (interactive)
  (if (and kb/theme-original-font (eq kb/theme-original-font (frame-parameter nil 'font)))
      (kb/set-window-font)
    (set-frame-parameter nil 'font kb/theme-original-font)))

(define-key global-map (kbd "C-c o") 'kb/switch-font)
;;--------------------------------------------------------------------------------
(defun kb/load-grapics-theme ()
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

;; (kb/activate-theme)
;; (add-function :after after-focus-change-function #'kb/activate-theme)
;; (add-hook 'after-make-frame-functions-hook 'kb/load-frame-theme)
(leaf custom
  :doc "tools for declaring and initializing options"
  :tag "builtin" "faces" "help"
  :added "2022-11-04"
  :init
  (add-to-list 'initial-frame-alist '(font . "Hack"))
  (add-to-list 'default-frame-alist '(font . "Hack"))
  :config
  (load-theme kb/window-theme t)
  (kb/set-window-font)
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

(defconst kb/c++-mavenir
  '(
    (fill-column . 100)
    (c-basic-offset . 4)
    (tab-width . 4)
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((innamespace . 0)
			(inline-open . 0)
			(substatement-open . 0)
			(arglist-intro . ++)
			;; (func-decl-cont . ++)
			(statement-cont . ++)
			(statement-case-open . 0)
			;; (statement-case-intro  . 0)
			(case-label . +)
			))
    (c-hanging-braces-alist . ((brace-list-open . before)
			       (brace-entry-open . before)
			       (substatement-open . before)
			       ;; (namespace-open . before)
			       ))
    )
  "Style used for C++ source editing at mavenir.")

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
  :hook (diff-mode-hook . 'kb/whitespace-diff-setup)
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
		  big-indent
		  space-before-tab
		  ))
    )
  :hook (c-mode-common-hook . kb/whitespace-progmode-setup)
)

(defun kb/checkpatch-enable ()
  "Add checkpatch for diff files if project provides checkpatch.pl script."
  (if (file-exists-p "./scripts/checkpatch.pl")
      (progn (print "setting compile-command")
	     (set (make-local-variable 'compile-command)
		  (concat "./scripts/checkpatch.pl --emacs "
			  (buffer-file-name))))
    (print "checkpatch not found")))
(add-hook 'diff-mode-hook 'kb/checkpatch-enable)

;; Highlight uncommitted changes
(leaf diff-hl
  :ensure t
  :hook ((prog-mode-hook vc-dir-mode-hook) . turn-on-diff-hl-mode))

;; vc-hooks
(leaf vc
  :doc "Version Control settings"
  :tag "builtin"
  :custom (vc-handled-backends . '(git svn)))

(leaf dsvn
  :ensure t
  :require vc-svn
  :config
  (autoload 'svn-status "dsvn" "Run `svn status'." t)
  (autoload 'svn-update "dsvn" "Run `svn update'." t)
  )

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

;;--------------------------------------------------------------------------------
;; Speedbar setup
;;--------------------------------------------------------------------------------
(leaf speedbar
  :doc "quick access to files and tags in a frame"
  :tag "builtin"
  :added "2022-11-01"
  :custom ((speedbar-indentation-width . 1)    ;; number of spaces used for indentation
           )
  :bind (("<f4>" . speedbar-get-focus)
         ;; bind the arrow keys in the speedbar tree
         (:speedbar-mode-map ("<right>" . speedbar-expand-line)
                             ("<left>" . 'speedbar-contract-line))
         )
  :config (speedbar-add-supported-extension '(".tex" ".bib" ".w"))
  )
;; speedup tramp
(leaf tramp
  :disabled t
  ;; :ensure tramp
  ;; :pin "gnu"
  :custom
  (tramp-verbose . 1)
  (vc-ignore-dir-regexp .
 	                '(format "\\(%s\\)\\|\\(%s\\)"
 		                 vc-ignore-dir-regexp
 		                 tramp-file-name-regexp))
  ;; :config
  ;; (tramp-recompile-elpa)
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
		     (filename . ".md"))
	  ("Emacs" (or (name . "^\\*scratch\\*$")
		       (name . "^\\*Messages\\*$")))
	  ("Gnus" (or (mode . message-mode)
		      (mode . bbdb-mode)
		      (mode . mail-mode)
		      (mode . gnus-group-mode)
		      (mode . gnus-summary-mode)
		      (mode . gnus-article-mode)
		      (name . "^\\.bbdb$")
		      (name . "^\\.newsrc-dribble"))))))))

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
(leaf banner-comment
  :ensure t
  :bind ("M-'" . banner-comment)
  )

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
  :custom ((anzu-mode-lighter . "")
           (anzu-deactivate-region . t)
           (anzu-search-threshold . 1000)
           (anzu-replace-threshold . 50)
           (anzu-replace-to-string-separator . " => ")
           )
  :bind ((([remap query-replace] . #'anzu-query-replace)
          ([remap query-replace-regexp] . #'anzu-query-replace-regexp))
         (:isearch-mode-map
          ([remap isearch-query-replace] . #'anzu-query-replace)
          ([remap isearch-query-replace-regexp] . #'anzu-query-replace-regexp)))
  ;; :custom-face
  ;; (anzu-mode-line . '((t (:inherit anzu-mode-line :foreground "yellow" :weight "bold"))))
  :config
  (global-anzu-mode +1))

(leaf golden-ratio
  :disabled t
  :ensure t
  :config (golden-ratio-mode))

(leaf editorconfig
  :ensure t
  :blackout " EC"
  :config (editorconfig-mode 1))

(leaf posframe
  :ensure t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :added "2022-11-16"
  :emacs>= 24.3
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
  :config
  (leaf flycheck-hydra
    :after flycheck hydra
    :config
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
  (leaf flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup))
  (leaf flycheck-clang-tidy
    :ensure t
    :after flycheck projectile
    :config (flycheck-clang-tidy-setup))
  (leaf flycheck-google-cpplint
    :ensure t
    :after flycheck
    :defun flycheck-add-next-checker
    :require flycheck-google-cpplint
    :config
    (flycheck-add-next-checker 'c/c++-cppcheck
			       'c/c++-googlelint 'append))
  (leaf flycheck-projectile
    :after flycheck projectile
    :ensure t
    :require flycheck-projectile)
  (leaf avy-flycheck
    :ensure t
    :hook (flycheck-mode-hook . avy-flycheck-setup))
  (leaf flycheck-posframe
    :doc "Show flycheck error messages using posframe.el"
    :req "flycheck-0.24" "emacs-26" "posframe-0.7.0"
    :tag "emacs>=26"
    :url "https://github.com/alexmurray/flycheck-posframe"
    :added "2022-11-16"
    :emacs>= 26
    :ensure t
    :after flycheck posframe
    :hook flycheck-mode-hook)
  )

;;================================================================================
;; Spell checking
;;================================================================================
(leaf ispell
  :doc "interface to spell checkers"
  :tag "builtin"
  :added "2022-11-03"
  :disabled t
  ;; :when ispell-program-name
  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
  ;; Hack ispell-local-dictionary-alist instead.
  :custom ((ispell-extra-args . '(flyspell-detect-ispell-args t))
           ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
           )
  :config
  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  ;; whatever spell checker I use, I always use English dictionary
  ;; I prefer use aspell because:
  ;; 1. aspell is older
  ;; 2. looks Kevin Atkinson still get some road map for aspell:
  ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
  (defun flyspell-detect-ispell-args (&optional run-together)
    "If RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
        ;; Force the English dictionary for aspell
        ;; Support Camel Case spelling check (tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=en_US"))
        (when run-together
	  (cond
	   ;; Kevin Atkinson said now aspell supports camel case directly
	   ;; https://github.com/redguardtoo/emacs.d/issues/796
	   ((string-match-p "--camel-case"
			    (shell-command-to-string (concat ispell-program-name " --help")))
	    (setq args (append args '("--camel-case"))))

	   ;; old aspell uses "--run-together". Please note we are not dependent on this option
	   ;; to check camel case word. wucuo is the final solution. This aspell options is just
	   ;; some extra check to speed up the whole process.
	   (t
	    (setq args (append args '("--run-together" "--run-together-limit=16")))))))
       ((string-match "hunspell$" ispell-program-name)
        ;; Force the English dictionary for hunspell
        (setq args "-d en_US")))
      args))

  ;; ispell-program-name
  (cond
   ((executable-find "aspell")
    ;; you may also need `ispell-extra-args'
    (setq ispell-program-name "aspell"))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")

    ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
    ;; it's also used as the key to lookup ispell-local-dictionary-alist
    ;; if we use different dictionary
    (setq ispell-local-dictionary nil)
    (setq ispell-local-dictionary-alist
	  '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
	    ("pl_PL" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "pl_PL") nil utf-8))))
   (t (setq ispell-program-name nil)))

  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; use emacs original arguments
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      ;; restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (defun text-mode-hook-setup ()
    ;; Turn off RUN-TOGETHER option when spell check text-mode
    (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
  :hook (text-mode-hook . text-mode-hook-setup)
  )

(leaf flycheck-aspell
  :doc "Aspell checker for flycheck"
  :req "flycheck-28.0" "emacs-25.1"
  :tag "aspell" "spell" "flycheck" "wp" "emacs>=25.1"
  :url "https://github.com/leotaku/flycheck-aspell"
  :added "2022-11-03"
  ;; :emacs>= 25.1
  :disabled t
  :ensure t
  :after flycheck
  :advice (:after ispell-pdict-save flycheck-maybe-recheck)
  :config
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
  )

(leaf unicode-fonts :ensure t)

;; http://unifoundry.com/pub/unifont/unifont-14.0.01/font-builds/unifont-14.0.01.ttf
(defvar kb/fonts '((:url "http://unifoundry.com/pub/unifont/unifont-14.0.03/font-builds/"
			 :fonts "unifont-14.0.03.otf"
			 :method 'download)
		   (:url "https://github.com/source-foundry/Hack/releases/download/v3.003/"
			 :fonts "Hack-v3.003-ttf.tar.xz"
			 :archive "Hack-v3.003-ttf.tar.xz"
			 :method 'tarxz)
		   (:url "https://github.com/google/fonts/raw/main/ofl/cantarell/"
			 :fonts ("Cantarell-BoldOblique.ttf"
				 "Cantarell-Bold.ttf"
				 "Cantarell-Oblique.ttf"
				 "Cantarell-Regular.ttf")
			 :metod 'download)
		   ) "List of font urls which should be installed.")

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
;; 	      (let* ((fonts-arg (plist-get kb/fonts :fonts))
;; 		     (files (if (atom fonts-arg) (list fonts-arg) (fonts-arg))))
;; 		(mapc (lambda (file)
;; 			(let* ((file-url (concat (plist-get font-url :url) file))
;; 			       (file-dst (expand-file-name file font-dest))
;; 			       )
;; 			  (prin1 "file-url: ")
;; 			  (prin1 file-url)
;; 			  (prin1 " -> file-dst: ")
;; 			  (print file-dst)
;; 			  (url-copy-file file-url
;; 				       file-dst t)
;; 			)) files)))
;; 	    kb/fonts)
;;       (when known-dest?
;;         (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
;;         (shell-command-to-string (format "fc-cache -f -v")))
;;       (message "%s Successfully %s `all-the-icons' fonts to `%s'!"
;;                (all-the-icons-wicon "stars" :v-adjust 0.0)
;;                (if known-dest? "installed" "downloaded")
;;               font-dest))))

(leaf undo-tree :ensure t
  :blackout t
  :disabled t
  :custom ((undo-tree-auto-save-history . nil))
  :config (global-undo-tree-mode)
  ;; (defadvice undo-tree-make-history-save-file-name
  ;;     (after undo-tree activate)
  ;;   (setq ad-return-value (concat ad-return-value ".xz")))
  )

(leaf beacon
  :ensure t
  ;; :pin "melpa"
  :config (beacon-mode 1)
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
  :hook (org-mode-hook . kb/org-mode-setup)
  (org-mode-hook . kb/org-font-setup)
  :custom ((org-ellipsis . " ▾")
           (org-hide-leading-stars . t)
           (org-agenda-start-with-log-mode . t)
           (org-log-done . 'time)
           (org-log-into-drawer . t)
           (org-src-fontify-natively . t)
           (org-export-with-smart-quotes . t)
           ;; (setq org-src-fontify-natively t)
           ;; (setq org-export-with-smart-quotes nil)
           (org-html-htmlize-output-type . nil)
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
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
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
  ;; 			      ("cpp" (:background "grey5" :foreground "chartreuse"))
  ;; 			      ("protobuf" (:background "grey5" :foreground "chartreuse"))
  ;; 			      ))
  )
  )

(leaf org-bullets
  :doc "Show bullets in org-mode as UTF-8 characters"
  :url "https://github.com/integral-dw/org-bullets"
  :added "2022-10-31"
  :ensure t
  :hook org-mode-hook
  :custom
  (org-bullets-bullet-list . '("◉" "○" "●" "○" "●" "○" "●")))

(leaf htmlize
  :doc "Convert buffer text and decorations to HTML."
  :tag "extensions" "hypermedia"
  :url "https://github.com/hniksic/emacs-htmlize"
  :added "2022-10-31"
  :ensure t)

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
  :require ox-mediawiki)
(leaf ox-pukiwiki
  :doc "Pukiwiki Back-End for Org Export Engine"
  :req "org-8.1"
  :tag "pukiwiki" "org"
  :url "https://github.com/yashi/org-pukiwiki"
  :added "2022-10-31"
  :ensure t
  :after org
  :require ox-pukiwiki)
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
  ;; :emacs>= 24.4
  :ensure t
  :after org)
(leaf ox-wk
  :doc "Wiki Back-End for Org Export Engine"
  :req "emacs-24.4" "org-8.3"
  :tag "wiki" "wp" "org" "emacs>=24.4"
  :url "https://github.com/w-vi/ox-wk.el"
  :added "2022-10-31"
  ;; :emacs>= 24.4
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

(leaf deft
  :doc "quickly browse, filter, and edit plain text notes"
  :tag "notational velocity" "simplenote" "notes" "plain text"
  :url "https://jblevins.org/projects/deft/"
  :added "2022-10-31"
  :disabled t
  :ensure t
  :custom ((deft-directory . org-journal-dir)
           (deft-recursive . t)))

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-26.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=26.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))
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

(leaf amx :ensure t)
(leaf company
  :ensure t
  :hook (after-init-hook . global-company-mode)
  :custom (
           (company-minimum-prefix-length . 1)
           (company-idle-delay . 0.0)
           (company-clang-excecutable . "clang")
           )
  :bind ((([remap dabbrev-expand] . company-dabbrev))
         ;; ((:map c-mode-map ("TAB" . company-complete))
         ;;  (:map c++-mode-map ("TAB" . company-complete)))
         (:company-active-map ("<tab>" . company-complete-selection))
         ;;(:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
         )
  :config
  (defun kb/company-hook ()
    "Hook to setup company mode"
    (global-company-mode))

  (delete 'company-semantic company-backends)
  (delete 'company-oddmuse company-backends)
  (delete 'company-gtags company-backends)
  (if (fboundp 'yas-expand)
      (add-hook 'c-mode-common-hook (lambda ()
                                      (message "Yo this is yasnippet backend")
                                      (add-to-list (make-local-variable 'company-backends) 'company-yasnippet))))
  :config
  (leaf company-statistics
    :ensure t
    :config
    (company-statistics-mode))
  (leaf company-posframe
    :ensure t
    :after posframe company
    :custom ((company-posframe-lighter . ""))
    :config
    (company-posframe-mode 1)
    )
  (leaf company-quickhelp
    :ensure t
    :disabled t
    :after company
    :bind (:company-active-map ("C-c h" . #'company-quickhelp-manual-begin))
    :config
    (company-quickhelp-mode 1)
    )

  (leaf company-c-headers
    :doc "Company mode backend for C/C++ header files"
    :req "emacs-24.1" "company-0.8"
    :tag "company" "development" "emacs>=24.1"
    :added "2022-10-31"
    :emacs>= 24.1
    :ensure t
    :disabled t
    :after company
    :config
    (add-to-list 'company-backends 'company-c-headers)
    )
)

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
  :doc "Format code using clang-format"
  :req "cl-lib-0.3"
  :tag "c" "tools"
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
    :ensure t
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
  ;; (lsp-enable-xref t)
  (lsp-log-io . nil)
  (lsp-idle-delay . 1.0)
  (lsp-keymap-prefix . "C-c l")
  (lsp-completion-provider . :capf)
  (lsp-headerline-breadcrumb-enable . t)
  (lsp-headerline-breadcrumb-segments . '(symbols project))
  (lsp-enable-snippet . nil)

  :hook ((c-mode-common-hook . lsp-deferred))
  :bind
         ;; :bind (:map lsp-mode-map ("M-." . lsp-find-declaration))
         (:lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  
  :config
  (defun kb/lsp-breadcrumb-face-setup ()
    "Fix headerlime colors for breadcrumbs"
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil :foreground "yellow" :background nil   :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil  :foreground "PaleGreen" :background nil :width 'extra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-separator-face nil :foreground "green" :background nil :weight 'ultra-bold :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil :foreground "green" :background nil :weight 'light :width 'ultra-condensed)
    (set-face-background 'header-line "black")
    )
  :hook
  (lsp-headerline-breadcrumb-mode-hook . kb/lsp-breadcrumb-face-setup)
  
  :config
  (leaf lsp-mode-which-key
    :after which-key lsp-mode 
    :config (lsp-enable-which-key-integration t))

  (leaf lsp-yasnippet
    :after yasnippet lsp-mode
    :custom (lsp-enable-snippet . t))
  
  ;; (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
  )
(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :added "2022-10-31"
  :emacs>= 26.1
  :ensure t
  ;; :after lsp-mode markdown-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:lsp-ui-mode-map
	 ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode
  :custom ((lsp-ui-doc-delay . 1)
           (lsp-ui-doc-enable . nil)
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-position . 'bottom)
           (lsp-ui-imenu-enable . t)
           (lsp-ui-peek-enable . t)
           (lsp-ui-sideline-enable . t)
           (lsp-ui-sideline-enable . t)
           )
  )
(leaf lsp-ivy
  :doc "LSP ivy integration"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
  :tag "debug" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-lsp/lsp-ivy"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode ivy
  :commands lsp-ivy-workspace-symbol)

(leaf cmake-mode
  :doc "major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2022-10-31"
  :emacs>= 24.1
  :ensure t
  ;; :if (package-installed-p 'cmake-mode)
  ;; :defines (cmake-tab-width)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode-hook . ((lambda ()
		                      (message "CmakeMode custom")
		                      (setq fill-column 80)
		                      (auto-fill-mode)
		                      (setq cmake-tab-width 4)
		                      (setq indent-tabs-mode nil))
                            (lsp-deferred)))
  :config
  (leaf cmake-font-lock
    :doc "Advanced, type aware, highlight support for CMake"
    :req "cmake-mode-0.0"
    :tag "languages" "faces"
    :url "https://github.com/Lindydancer/cmake-font-lock"
    :added "2022-10-31"
    :ensure t
    ;; :if (package-installed-p 'cmake-mode)
    :after cmake-mode
    ;; :hook (cmake-mode-hook . cmake-font-lock-activate)
    )
  )
(leaf treemacs
  :doc "A tree style file explorer package"
  :req "emacs-26.1" "cl-lib-0.5" "dash-2.11.0" "s-1.12.0" "ace-window-0.9.0" "pfuture-1.7" "hydra-0.13.2" "ht-2.2" "cfrs-1.3.2"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-10-31"
  ;; :emacs>= 26.1
  :ensure t
  ;; :after ace-window pfuture hydra cfrs
  :bind ([f8] . treemacs)
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
  :config
  (leaf lsp-treemacs
    :doc "LSP treemacs"
    :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
    :tag "languages" "emacs>=26.1"
    :url "https://github.com/emacs-lsp/lsp-treemacs"
    :added "2022-10-31"
    ;; :emacs>= 26.1
    :ensure t
    :after treemacs lsp-mode
    :commands lsp-treemacs-errors-list)
  )
(leaf dap-mode
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

(leaf ztree
  :doc "Text mode directory tree"
  :req "cl-lib-0"
  :tag "tools" "files"
  :url "https://github.com/fourier/ztree"
  :added "2022-10-31"
  :ensure t)

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :added "2022-10-31"
  :emacs>= 24.4
  :ensure t
  ;; :defer 0
  :blackout t
  :custom (which-key-idle-delay . 1)
  :config (which-key-mode)
  (add-to-list 'which-key-replacement-alist
	           '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))

  (add-to-list 'which-key-replacement-alist
	           '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))
  (which-key-setup-side-window-right)
  :config
  (leaf hercules
    :doc "An auto-magical, which-key-based hydra banisher."
    :req "emacs-24.4" "which-key-3.3.2"
    :tag "convenience" "emacs>=24.4"
    :url "https://gitlab.com/jjzmajic/hercules"
    :added "2022-10-31"
    :emacs>= 24.4
    :ensure t
    :after which-key)

  (leaf which-key-posframe
    :doc "Using posframe to show which-key"
    :req "emacs-26.0" "posframe-0.4.3" "which-key-3.3.2"
    :tag "tooltip" "bindings" "convenience" "emacs>=26.0"
    :url "https://github.com/yanghaoxie/which-key-posframe"
    :added "2022-10-31"
    :emacs>= 26.0
    :ensure t
    :after posframe which-key
    :custom (which-key-posframe-font . "Liberation Mono"))
    :config (which-key-posframe-mode)
  )
(leaf ninja-mode
  :doc "Major mode for editing .ninja files"
  :req "emacs-24"
  :tag "emacs>=24"
  :added "2022-10-31"
  :emacs>= 24
  :ensure t)
(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :url "https://github.com/yoshiki/yaml-mode"
  :added "2022-10-31"
  :emacs>= 24.1
  :ensure t)
(leaf ietf-docs
  :doc "Fetch, Cache and Load IETF documents"
  :tag "rfc" "ietf"
  :url "https://github.com/choppsv1/ietf-docs"
  :added "2022-10-31"
  :ensure t
  :bind (("C-c k o" . ietf-docs-open-at-point)))
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
  :ensure t
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
  ;; (all-the-icons-install-fonts t)
  (leaf all-the-icons-dired
    :doc "Shows icons for each file in dired mode"
    :req "emacs-26.1" "all-the-icons-2.2.0"
    :tag "dired" "icons" "files" "emacs>=26.1"
    :url "https://github.com/wyuenho/all-the-icons-dired"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :after all-the-icons
    :hook dired-mode-hook)
  (leaf all-the-icons-ibuffer
    :doc "Display icons for all buffers in ibuffer"
    :req "emacs-24.4" "all-the-icons-2.2.0"
    :tag "ibuffer" "icons" "convenience" "emacs>=24.4"
    :url "https://github.com/seagle0128/all-the-icons-ibuffer"
    :added "2022-10-31"
    :emacs>= 24.4
    :ensure t
    :after all-the-icons
    :config
    (with-eval-after-load 'ibuffer
      '(all-the-icons-ibuffer-mode 1)))
  (leaf treemacs-all-the-icons
    :doc "all-the-icons integration for treemacs"
    :req "emacs-26.1" "all-the-icons-4.0.1" "treemacs-0.0"
    :tag "emacs>=26.1"
    :url "https://github.com/Alexander-Miller/treemacs"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :after all-the-icons treemacs)
  (leaf all-the-icons-ivy
    :doc "Shows icons while using ivy and counsel"
    :req "emacs-24.4" "all-the-icons-2.4.0" "ivy-0.8.0"
    :tag "faces" "emacs>=24.4"
    :added "2022-10-31"
    :emacs>= 24.4
    :ensure t
    :after all-the-icons ivy
    :config
    (all-the-icons-ivy-setup))
  (leaf all-the-icons-ivy-rich
    :doc "Better experience with icons for ivy"
    :req "emacs-25.1" "ivy-rich-0.1.0" "all-the-icons-2.2.0"
    :tag "ivy" "icons" "convenience" "emacs>=25.1"
    :url "https://github.com/seagle0128/all-the-icons-ivy-rich"
    :added "2022-10-31"
    :emacs>= 25.1
    :ensure t
    :after ivy-rich all-the-icons
    :config (all-the-icons-ivy-rich-mode 1))
  (leaf all-the-icons-completion
    :doc "Add icons to completion candidates"
    :req "emacs-26.1" "all-the-icons-5.0"
    :tag "lisp" "convenient" "emacs>=26.1"
    :url "https://github.com/iyefrat/all-the-icons-completion"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :after all-the-icons
    :hook (marginalia-mode-hook . #'all-the-icons-completion-marginalia-setup)
    :config (all-the-icons-completion-mode)
    )
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
  ;; THe :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package/leaf such that the
  ;; mode gets enabled right away. Note that this forces loading the
  ;; package
  (marginalia-mode +1))

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

(leaf counsel
  :ensure t
  :custom ((counsel-find-file-at-point . t)
           (counsel-find-file-ignore-regexp . (regexp-opt completion-ignored-extensions)))
  :bind (("<f2> j". counsel-set-variable)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f7>" . counsel-recentf)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c L" . counsel-git-log)
         ("C-c J" . counsel-file-jump)
         ("C-x l" . counsel-locate)
         ;; ("C-c t" . counsel-load-theme)
         ("C-c C-o" . counsel-imenu)
         ([remap insert-register] . counsel-register)
         (:minibuffer-local-map
          ("C-r" . counsel-minibuffer-history)))
  :config
  (counsel-mode)
  :config
  (leaf counsel-tramp
    :ensure t
    :after counsel)
  (leaf counsel-projectile
    :ensure t
    :config(counsel-projectile-mode))
  (leaf counsel-ag-popup :ensure t)
  (leaf counsel-edit-mode :ensure t
    :config (counsel-edit-mode-setup-ivy))
  (leaf counsel-jq
    :doc "Live preview of \"jq\" queries using counsel"
    :req "swiper-0.12.0" "ivy-0.12.0" "emacs-24.1"
    :tag "matching" "data" "convenience" "emacs>=24.1"
    :url "https://github.com/200ok-ch/counsel-jq"
    :added "2022-10-31"
    :emacs>= 24.1
    :when (executable-find "jq")
    :ensure t
    :after swiper ivy)

  (leaf helpful
    :ensure t
    :after counsel
    :custom
    (counsel-describe-function-function . #'helpful-callable)
    (counsel-describe-variable-function . #'helpful-variable)
    (counsel-describe-symbol-function . #'helpful-symbol)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key)
    )
  )

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2022-10-31"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :custom ((ivy-use-virutal-buffers . t)
           (ivy-count-format . "(%d/%d) ")
           (enable-recursive-minibuffers . t)
           (ivy-use-selectable-prompt . t)
           )
  :bind (("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ;; ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 ;; ("C-c m" . kb/ivy-switch-project)
	 ("C-c n" . kb/ivy-switch-git)
	 )
  :config
  (defun kb/ivy-switch-project ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
	 (cons (abbreviate-file-name (projectile-project-root))
	       (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name))

  (ivy-set-actions
   'kb/ivy-switch-project
   '(("d" dired "Open Dired in project's directory")
     ("v" counsel-projectile-switch-project-action-vc "Open project root in vc-dir or magit")
     ("e" counsel-projectile-switch-project-action-run-eshell "Switch to Eshell")
     ("f" counsel-projectile-switch-project-action-find-file "Find file in project")
     ("g" counsel-projectile-switch-project-action-grep "Grep in projects")
     ("a" counsel-projectile-switch-project-action-ag "AG in projects")
     ("c" counsel-projectile-switch-project-action-compile "Compile project")
     ("r" counsel-projectile-switch-project-action-remove-known-project "Remove project(s)")))

  (defun kb/ivy-switch-git ()
    (interactive)
    (ivy-read
     "Switch GIT repository: "
     (magit-list-repos)
     :action #'magit-status))

  (ivy-set-actions
   'kb/ivy-switch-git
   '(("d" dired "Open Dired in GIT directory")
     ("f" magit-fetch "Fetch")
     ("F" magit-find-file "Find file in git")))
  
  (ivy-mode 1)

  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.4"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :added "2022-10-31"
    :emacs>= 24.5
    :ensure t
    :after ivy
    :ensure t
    :bind ([remap isearch-forward] . swiper-isearch)
    )
  (leaf ivy-posframe
    :doc "Using posframe to show Ivy"
    :req "emacs-26.0" "posframe-1.0.0" "ivy-0.13.0"
    :tag "ivy" "matching" "convenience" "abbrev" "emacs>=26.0"
    :url "https://github.com/tumashu/ivy-posframe"
    :added "2022-10-31"
    :emacs>= 26.0
    :ensure t
    :after posframe ivy
    ;; :defines (ivy-posframe-display-functions-alist)
    :custom (
             ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-right)
             ;; 					       (t . ivy-posframe-display-at-frame-top-center)))
             (ivy-posframe-height-alist . '((swiper-isearch . 10)
				            (swiper	    . 10)
                                            (t		    . 40)))
             (ivy-posframe-display-functions-alist .
	                                           '((swiper			. ivy-display-function-fallback)
                                                     (complete-symbol		. ivy-posframe-display-at-point)
                                                     (counsel-M-x		. ivy-posframe-display-at-window-bottom-left)
	                                             (ivy-switch-buffer		. ivy-posframe-display-at-window-center)
	                                             (counsel-find-file		. ivy-posframe-display-at-window-bottom-left)
	                                             (counsel-describe-variable	. ivy-posframe-display-at-frame-top-right)
	                                             (t				. ivy-posframe-display-at-frame-top-right)
                                                     ;; (t               . ivy-posframe-display)
	                                             ))
             (ivy-posframe-parameters . '((left-fringe . 8)
                                          (right-fringe . 8)))
             ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
             ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
             ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
             ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
             )
    :config
    ;; display at `ivy-posframe-style'
    (defun ivy-posframe-display-at-frame-top-right (str)
      (ivy-posframe--display str #'posframe-poshandler-frame-top-right-corner))

    (ivy-posframe-mode 1)
    )
  (leaf ivy-rich
    :doc "More friendly display transformer for ivy"
    :req "emacs-25.1" "ivy-0.13.0"
    :tag "ivy" "convenience" "emacs>=25.1"
    :url "https://github.com/Yevgnen/ivy-rich"
    :added "2022-10-31"
    :emacs>= 25.1
    :ensure t
    :after ivy
    :hook ivy-mode-hook)
  (leaf ivy-xref
    :doc "Ivy interface for xref results"
    :req "emacs-25.1" "ivy-0.10.0"
    :tag "emacs>=25.1"
    :url "https://github.com/alexmurray/ivy-xref"
    :added "2022-10-31"
    :emacs>= 25.1
    :ensure t
    :after ivy
    :custom (xref-show-xrefs-function . 'ivy-xref-show-xrefs))
  (leaf ivy-avy
    :doc "Avy integration for Ivy"
    :req "emacs-24.5" "ivy-0.13.4" "avy-0.5.0"
    :tag "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :added "2022-10-31"
    :emacs>= 24.5
    :ensure t
    :after ivy avy)
  (leaf ivy-hydra
    :doc "Additional key bindings for Ivy"
    :req "emacs-24.5" "ivy-0.13.4" "hydra-0.14.0"
    :tag "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :added "2022-10-31"
    :emacs>= 24.5
    :ensure t
    :after ivy hydra)
  (leaf ivy-emoji
    :doc "Insert emojis with ivy"
    :req "emacs-26.1" "ivy-0.13.0"
    :tag "convenience" "ivy" "emoji" "emacs>=26.1"
    :url "https://github.com/sbozzolo/ivy-emoji.git"
    :added "2022-10-31"
    :emacs>= 26.1
    :ensure t
    :after ivy
    :config
    (set-fontset-font t 'symbol
                      (font-spec :family "Noto Color Emoji") nil 'prepend)
    ;; (set-fontset-font t 'symbol
    ;;                     (font-spec :family "Symbola") nil 'prepend)

    ;; Download Noto Color Emoji
    ;; https://noto-website-2.storage.googleapis.com/pkgs/NotoColorEmoji-unhinted.zip
    ;; Unzip -> copy NotoColorEmoji.ttf to ~/.local/share/fonts/
    ;; Run fc-cache -fv
    )
  )

(unless (executable-find "cargo") (warn "cargo Rust package manager not found"))
(leaf fuz
  :doc "Fast and precise fuzzy scoring/matching utils"
  :req "emacs-25.1"
  :tag "lisp" "emacs>=25.1"
  :url "https://github.com/cireu/fuz.el"
  :added "2022-10-31"
  :emacs>= 25.1
  :when (executable-find "cargo")
  :ensure t
  :require fuz
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  (leaf ivy-fuz
    :doc "Integration between fuz and ivy."
    :req "emacs-25.1" "fuz-1.3.0" "ivy-0.13.0"
    :tag "convenience" "emacs>=25.1"
    :url "https://github.com/Silex/ivy-fuz.el"
    :added "2022-10-31"
    :emacs>= 25.1
    :ensure t
    :after fuz ivy)
  )

(leaf smart-compile
  :doc "an interface to `compile'"
  :tag "unix" "tools"
  :added "2022-10-31"
  :ensure t
  :require smart-compile)

(leaf zygospore
  :doc "reversible C-x 1 (delete-other-windows)"
  :url "https://github.com/louiskottmann/zygospore.el"
  :added "2022-10-31"
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Use silversearcher-ag if available.
(unless (executable-find "ag") (warn "silversearcher-ag not found"))
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

;; Use ripgrep if available
(unless (executable-find "rg") (warn "ripgrep not found"))
(leaf rg
  :doc "A search tool based on ripgrep"
  :req "emacs-25.1" "transient-0.3.0" "wgrep-2.1.10"
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

;; :delight '(:eval (concat " " (projectile-project-name)))
(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1"
  :tag "convenience" "project" "emacs>=25.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t
  :blackout t ; (projectile-project-name);  '(:eval (concat " " (projectile-project-name)))
  :commands projectile-project-p
  :custom ((projectile-indexing-method . 'alien)
           (projectile-completion-system . 'ivy)
           (projectile-enable-caching . t)
           (projectile-sort-order . 'recently-active)
           )
  :bind (:projectile-mode-map
	 ("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map))
  :config
  (message "Running projectile mode")
  (projectile-mode)
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

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
  :custom ((python-shell-interpreter . '(executable-find "python3"))
           (py-python-command-args . '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
           (py-force-py-shell-name-p . t)
           (py-shell-switch-buffers-on-execute-p . t)
           (py-switch-buffers-on-execute-p . t)
           (py-split-windows-on-execute-p . nil)
           (py-smart-indentation . t)
           ))
(leaf lsp-python-ms
  :doc "The lsp-mode client for Microsoft python-language-server"
  :req "emacs-25.1" "lsp-mode-6.1"
  :tag "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-lsp/lsp-python-ms"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t
  :hook (python-mode-hook . (lambda ()
			      (require 'lsp-python-ms)
			      (lsp)))
  :init (setq lsp-python-ms-auto-install-server t))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "compat-28.1.1.2" "dash-20210826" "git-commit-20220222" "magit-section-20220325" "transient-20220325" "with-editor-20220318"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2022-10-31"
  :emacs>= 25.1
  :ensure t
  :commands magit-list-repos
  :after compat git-commit magit-section with-editor
  ;; :commands magit-status
  ;; :bind (:map global-map
  ;; 	      ("C-c n". magit-list-repos))
  :custom ((magit-display-buffer-function . #'magit-display-buffer-same-window-except-diff-v1)
           (magit-repository-directories . '(("~/projects" . 2)))
           (git-commit-summary-max-length . 50)
           )
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
  :config (transient-posframe-mode))

;; ASC screws any option for using gerrit package
;; (leaf gerrit
;;   :ensure t
;;   :custom
;;   ;;https://asc.bmwgroup.net/gerrit/#/c/1850299/6
;;   (gerrit-host . "asc.bmwgroup.net")  ;; is needed for REST API calls
;;   :hook ((magit-status-sections-hook . #'gerrit-magit-insert-status)
;;          (:global-map ("C-x i" . 'gerrit-upload-transient)
;;                       ("C-x o" . 'gerrit-download))))
;;--------------------------------------------------------------------------------
;; Optional packages
;;--------------------------------------------------------------------------------

(if (not (package-installed-p 'smartparens))
    (progn
      "smartparens not installed: Using builtin paren mode."
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

  (leaf smartparens
    :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."
    :req "dash-2.13.0" "cl-lib-0.3"
    :tag "editing" "convenience" "abbrev"
    :url "https://github.com/Fuco1/smartparens"
    :added "2022-10-31"
    :ensure t
    :blackout t
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
    ;;  (c-mode-common-hook . (lambda () (require 'smartparens-c)))
    ;;  (org-mode . (lambda () (require 'smartparens-org))))
    :custom ((sp-base-key-bindings . 'paredit)
             (sp-autoskip-closing-pair . 'always)
             (sp-hybrid-kill-entire-symbol . 'nil))
    :require smartparens-config
    :config
    (electric-pair-mode -1)
    ;; (smartparens-global-strict-mode 1)
    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1)
    )
  )
(leaf jira-markup-mode
  :doc "Emacs Major mode for JIRA-markup-formatted text files"
  :tag "markup" "jira"
  :url "https://github.com/mnuessler/jira-markup-mode"
  :added "2022-10-31"
  :ensure t
  :mode "\\.jira")
(leaf org-jira
  :doc "Syncing between Jira and Org-mode."
  :req "emacs-24.5" "cl-lib-0.5" "request-0.2.0" "dash-2.14.1"
  :tag "tracker" "bug" "org" "jira" "ahungry" "emacs>=24.5"
  :url "https://github.com/ahungry/org-jira"
  :added "2022-10-31"
  :emacs>= 24.5
  :ensure t
  :config
  (when (not (file-exists-p "~/.org-jira"))
    (make-directory "~/.org-jira"))
  ;; (setq jiralib-url "https://asc.bmwgroup.net/mgujira/") ;; https://asc.bmwgroup.net/mgujira/secure/Dashboard.jspa")
  )
(leaf copy-as-format
  :doc "Copy buffer locations as GitHub/Slack/JIRA etc... formatted code"
  :req "cl-lib-0.5"
  :tag "convenience" "tools" "asciidoc" "rst" "pod" "org-mode" "bitbucket" "gitlab" "hipchat" "jira" "slack" "github"
  :url "https://github.com/sshaw/copy-as-format"
  :added "2022-10-31"
  :ensure t)

(unless (executable-find "dot") (warn "dot command not found in system"))
(leaf graphviz-dot-mode
  :doc "Mode for the dot-language used by graphviz (att)."
  :req "emacs-25.0"
  :tag "att" "graphs" "graphviz" "dotlanguage" "dot-language" "dot" "mode" "emacs>=25.0"
  :url "https://ppareit.github.io/graphviz-dot-mode/"
  :added "2022-10-31"
  :when (executable-find "dot")
  :emacs>= 25.0
  :ensure t
  :config
  (when (executable-find "xdot")
    (customize-set-variable 'graphviz-dot-view-command "xdot %s"))
  :config
  ;; (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))
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

  :hydra (hydra-yas (:color blue :hint nil)
		            "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
		            ("d" yas-load-directory)
		            ("e" yas-activate-extra-mode)
		            ("i" yas-insert-snippet)
		            ("f" yas-visit-snippet-file :color blue)
		            ("n" yas-new-snippet)
		            ("t" yas-tryout-snippet)
		            ("l" yas-describe-tables)
		            ("g" yas/global-mode)
		            ("m" yas/minor-mode)
		            ("a" yas-reload-all))
  :config
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
  (leaf ivy-yasnippet
    :doc "Preview yasnippets with ivy"
    :req "emacs-24.1" "cl-lib-0.6" "ivy-0.10.0" "yasnippet-0.12.2" "dash-2.14.1"
    :tag "convenience" "emacs>=24.1"
    :url "https://github.com/mkcms/ivy-yasnippet"
    :added "2022-10-31"
    :emacs>= 24.1
    :ensure t
    :after ivy yasnippet)
  )

(defconst kb/plantuml-jar-path (expand-file-name "~/.java/libs/plantuml.jar")
  "Location where to search for plantuml.jar file.

Download and put appropriate file there.")
(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :req "dash-2.0.0" "emacs-25.0"
  :tag "ascii" "plantuml" "uml" "emacs>=25.0"
  :added "2022-10-31"
  :emacs>= 25.0
  :ensure t
  :config
  :custom ((plantuml-jar-path . #'kb/plantuml-jar-path)
           (plantuml-default-exec-mode . 'jar)
           (plantuml-indent-level . 4)
           (org-plantuml-jar-path . plantuml-jar-path)
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
  :after json-snatcher)

(defun kb/nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(leaf go-translate
  :doc "Translation framework supports multiple engines such as Google/Bing/DeepL"
  :req "emacs-27.1"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/lorniu/go-translate"
  :added "2022-11-01"
  :emacs>= 27.1
  :ensure t
  :custom
  (gts-translate-list . '(("en" "de") ("pl" "en") ("en" "pl") ("en" "zh") ("zh" "en") ("ko" "en")))
  ;; (setq gts-default-translator
  ;;   (gts-translator
  ;;        :picker (gts-prompt-picker)
  ;;        :engines (list (gts-bing-engine) (gts-google-engine) (gts-google-rpc-engine))
  ;;        :render (gts-buffer-render)))
  )
(message "Init finished")
;;; init.el ends here
