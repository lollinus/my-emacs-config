;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix -*-
;;; Commentary:
;; Load all configuration parts

;;; Code:
;; don't let Customize mess with my .emacs
(defconst kb/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun kb/emacs-subdirectory (d) (expand-file-name d kb/emacs-directory))

(setq custom-file (expand-file-name "custom.el" kb/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "karol.barski@tietoevry.com")


(defconst kb/environment-script (expand-file-name "environment.el" (kb/emacs-subdirectory "rc")))
(when (file-exists-p kb/environment-script)
  (load-file "~/.emacs.d/rc/environment.el"))

(add-to-list 'load-path (kb/emacs-subdirectory "rc"))

(when (file-exists-p (expand-file-name "rc-functions.el" (kb/emacs-subdirectory "rc")))
  (load "~/.emacs.d/rc/rc-functions.el"))

(setq gc-cons-threshold (* 50 1000 1000))
(setq large-file-warning-threshold 100000000)
(setq read-process-output-max (* 4 1024 1024))

(setq mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )
(setq tab-always-indent 'complete)

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
(setq completion-ignore-case t)
(setq load-prefer-newer t)

(setq mouse-highlight 10)
(setq make-pointer-invisible t)


;;--------------------------------------------------------------------------------
;; Default frame parameters
;;--------------------------------------------------------------------------------
;; (fullscreen-restore . fullheight)
;; (fullscreen . fullboth)
(defconst kb/frame-config '(
			    ;; (top . 1)
			    ;; (left . 1)
			    ;; (fullscreen . maximized)
			    (menu-bar-lines . 0)       ; turn menus off
			    (tool-bar-lines . 0)       ; disable toolbar
			    (scroll-bar-width . 10)
			    (vertical-scroll-bars . right)
			    (background-mode . dark))
  )

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))


(setq initial-frame-alist kb/frame-config)
(setq default-frame-alist kb/frame-config)

;; turn off blinking cursor
;; (setq blink-cursor-blinks 3)
;; (setq blink-cursor-delay 1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)



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

(require 'cl-lib)
(package-initialize)
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
    (add-to-list 'kb/package-archives (cons "elpa" (concat proto "://elpa.gnu.org/packages/")) t)
    )
  )

;;(add-to-list 'package-archives
;;           '("gnu" . "http://elpa.gnu.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'kb/package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'kb/package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'kb/package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
(setq package-archives kb/package-archives)

(defun kb/ensure-package-installed (&rest packages)
  "Assure every package is installed.
Return a list of installed packages or nil of every skipped package.

PACKAGES: list of packages to install."
  (mapcar
   (lambda (package)
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
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)


;;--------------------------------------------------------------------------------
;; My emacs config
;;--------------------------------------------------------------------------------
(setq inhibit-startup-screen t)
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
(setq global-mark-ring-max 5000)         ; increase mark ring to contains 5000 entries
(setq mark-ring-max 5000)                ; increase kill ring to contains 5000 entries
(column-number-mode +1)
(line-number-mode +1)
(size-indication-mode t)
(setq kill-ring-max 5000) ; increase kill-ring capacity
(setq kill-whole-line t)
(setq transient-mark-mode nil)
;; remap C-H to backspace
;; (normal-erase-is-backspace-mode t)
;; remap C-H to backspace
(add-hook 'terminal-init-xterm-hook (lambda () (normal-erase-is-backspace-mode t)))
(put 'set-goal-column 'disabled nil)
(define-key global-map (kbd "C-;") 'kill-whole-line)
(define-key global-map (kbd "M-o") 'other-window)

;; Use M-/ for `company` completion
(define-key input-decode-map "\e[1;2A" [S-up])

;; VC
(setq vc-follow-symlinks t)
;; help
(setq temp-buffer-resize-mode t)
;; image
(setq auto-image-file-mode 1)
;; time
;; (setq display-time-format "%H:%M %d/%m/%Y")
;; (setq display-time-24hr-format t)
;; (display-time)

;; kolorowanie składni
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; smerge-mode
(setq-default smerge-command-prefix (kbd "C-c v"))

(defun kb/set-buffer-eol-unix ()
  "Set current buffer EOL type to unix."
  (set-buffer-file-eol-type 'unix))
(defun kb/set-buffer-eol-dos ()
  "Set current buffer EOL type to dos."
  (set-buffer-file-eol-type 'dos))
(defun kb/set-buffer-eol-mac ()
  "Set current buffer EOL type to mac."
  (set-buffer-file-eol-type 'mac))

(define-key global-map (kbd "C-j") 'kb/join-line)
(define-key global-map (kbd "C-a") 'prelude-move-beginning-of-line)
;;  (define-key global-map (kbd "C-c i") 'indent-region-or-buffer)
;; (define-key global-map (kbd "M-o") 'prelude-smart-open-line)
(define-key global-map (kbd "%") 'match-parenthesis) ;; % key on paren moves cursor to matching paren
(define-key global-map (kbd "C-c T") 'kb/delete-trailing-whitespaces-and-untabify)
(define-key global-map (kbd "C-c u") 'kb/set-buffer-eol-unix)
(define-key global-map (kbd "C-c d") 'kb/set-buffer-eol-dos)
;; (define-key global-map (kbd "C-c m") 'set-buffer-eol-mac)
(define-key global-map (kbd "C-c C-d") 'insert/date-time)

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

;; (define-key global-map (kbd "") 'kb/update-env)

;; string-insert-rectangle is useful but not binded to any key by default
;; (define-key global-map (kbd "C-x r a") 'string-insert-rectangle) ;; use string-rectange instead "C-x r t"
(define-key global-map (kbd "C-x r l") 'rectangle-number-lines)

;; whitespace setup
(with-eval-after-load 'whitespace
  ;; (setq whitespace-style '(face trailing lines-tail newline empty indentation big-indent space-before-tab))
  (setq whitespace-style '(newline-mark newline))
  (setq whitespace-line-column nil)
  (setq whitespace-display-mappings '((space-mark 32 [183] [46])
				      (newline-mark 10 [9166 10])
				      (tab-mark 9 [9654 9] [92 9])))
  
  
  
  (define-key global-map (kbd "C-c w") 'whitespace-mode)
  (set-face-attribute 'whitespace-space nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil))

(global-whitespace-mode)

;; keep minibuffer history between session
;;(savehist-mode t)
;;(add-to-list 'savehist-additional-variables register-alist)

;; recentf mode
(recentf-mode t)
(define-key global-map (kbd "<f7>") 'recentf-open-files)

(define-key global-map (kbd "C-c o") 'kb/switch-font)

;;--------------------------------------------------------------------------------
;; Set Theme depending if emacs frame is inside TTY o GUI
;;--------------------------------------------------------------------------------
(use-package doom-themes
  :defines (doom-themes-treemacs-theme)
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; (load-theme 'doom-one t)
  ;; (doom-themes-visual-bell-config)
  ;; (when (fboundp doom-dark+-blue-modeline)
  ;; (setq doom-dark+-blue-modeline t)
  ;; (setq doom-dark+-padded-modeline nil)
  ;; )
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )
(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-hud t)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-default-coding-system 'utf-8)
  (setq doom-modeline-default-eol-type 0)
  (setq doom-modeline-vcs-max-length 24)
  )

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
;; (set-frame-font "Hack" nil t)

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
(kb/activate-theme)

(add-function :after after-focus-change-function #'kb/activate-theme)
(add-hook 'after-make-frame-functions-hook 'kb/load-frame-theme)

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
    (fill-column . 100)
    (c-basic-offset . 4)
    (tab-width . 4)
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((innamespace . 0)
			(inline-open . 0)
			(substatement-open . 0)
			(statement-cont . ++)))
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

(defun kb/c++-mode-hook ()
  "My style used while editing C++ sources."
  (c-add-style "kb/c++-style" kb/c++-style)
  (c-add-style "kb/c++-mavenir" kb/c++-mavenir t)
  (auto-fill-mode)
  (display-fill-column-indicator-mode))
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

(use-package hl-todo
  :bind
  (:map hl-todo-mode-map
	("C-c t p" . #'hl-todo-previous)
	("C-c t n" . #'hl-todo-next)
	("C-c t o" . #'hl-todo-occur)
	("C-c t i" . #'hl-todo-insert))
  :custom
  (add-to-list 'hl-todo-keyword-faces '("NOCOMMIT" . "#ff00ff"))
  :config
  (global-hl-todo-mode)
  )

;;--------------------------------------------------------------------------------
;; GDB
;;--------------------------------------------------------------------------------
(setq gdb-many-windows t)   ;; use gdb-many-windows by default
(setq gdb-show-main t) ;; Non-nil means display source file containing the main routine at startup

;; display-line-numbers
;; (setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width-start t)
;; Display line number for programming modes
(dolist (mode '(emacs-lisp-mode-hook
		lisp-mode-hook
		c-mode-common-hook
		prog-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode))))

(global-prettify-symbols-mode t)

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
(add-hook 'diff-mode-hook 'kb/whitespace-diff-setup)

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
(add-hook 'c-mode-common-hook 'kb/whitespace-progmode-setup)

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
(use-package diff-hl
  :hook
  (prog-mode . turn-on-diff-hl-mode)
  (vc-dir-mode . turn-on-diff-hl-mode))

;; vc-hooks
(setq vc-handled-backends '(git svn))

;; subword-mode
(add-hook 'c-mode-common-hook 'subword-mode)

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
(define-key global-map (kbd "<f4>") 'speedbar-get-focus)
(with-eval-after-load 'speedbar
  ;; number of spaces used for indentation
  (setq speedbar-indentation-width 1)
  ;; expand/collapse latex sections
  ;; (speedbar-add-supported-extension '(".tex" ".bib" ".w"))
  ;; jump to speedbar frame
  ;; bind the arrow keys in the speedbar tree
  (define-key speedbar-mode-map (kbd "<right>") 'speedbar-expand-line)
  (define-key speedbar-mode-map (kbd "<left>") 'speedbar-contract-line)
  )

;; speedup tramp
(use-package tramp
  :pin "elpa"
  :custom (setq vc-ignore-dir-regexp
		(format "\\(%s\\)\\|\\(%s\\)"
			vc-ignore-dir-regexp
			tramp-file-name-regexp))
  (setq tramp-verbose 1)
  (tramp-recompile-elpa)
  )
(use-package counsel-tramp
  :after counsel
  )

;;--------------------------------------------------------------------------------
;; iBuffer
;;--------------------------------------------------------------------------------
(define-key global-map (kbd "C-x C-b") #'ibuffer)
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
		      (name . "^\\.newsrc-dribble")))))))
(use-package ibuffer-projectile
  :hook (ibuffer . (lambda ()
		     (ibuffer-projectile-set-filter-groups)
		     (unless (eq ibuffer-sorting-mode 'alphabetic)
		       (ibuffer-do-sort-by-alphabetic))))
  :custom
  (ibuffer-formats '((mark modified ready-only " "
			   (name 18 18 :left :elide)
			   " "
			   (size 9 -1 :right)
			   " "
			   (mode 16 16 :left :elide)
			   " "
			   project-relative-file)))
  )
(use-package ibuffer-vc)


;;--------------------------------------------------------------------------------
;; Additional packages
;;--------------------------------------------------------------------------------
(use-package delight)

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2)
  )
(if (fboundp 'banner-comment)
    (use-package banner-comment
      :bind ("M-'" . banner-comment)
      )
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package anzu
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
  (:map isearch-mode-map
	([remap isearch-query-replace] . #'anzu-query-replace)
	([remap isearch-query-replace-regexp] . #'anzu-query-replace-regexp))
  (:map global-map
	([remap query-replace] . #'anzu-query-replace)
	([remap query-replace-regexp] . #'anzu-query-replace-regexp))
  )

(use-package winum
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-set-keymap-prefix (kbd "M-p"))
  (winum-mode)
  )
(use-package golden-ratio
  :config
  (golden-ratio-mode)
  )

(use-package editorconfig :ensure t
  :delight ""
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :defer 5
  :after hydra
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-checker-error-threshold 10000)
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
(use-package flycheck-clang-analyzer
  :config
  (with-eval-after-load 'flycheck
    'flycheck-clang-analyzer-setup))
(use-package flycheck-clang-tidy
  :config
  (with-eval-after-load 'flycheck
    'flycheck-clang-tidy-setup))
(use-package flycheck-google-cpplint
  :config
  (with-eval-after-load 'flycheck
    '(flycheck-add-next-checker 'c/c++-cppcheck
				'c/c++-googlelint 'append)))
(use-package flycheck-projectile)
(use-package avy-flycheck
  :hook (flycheck-mode . avy-flycheck-setup))

(use-package posframe)
(use-package flycheck-posframe
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-posframe)
    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  )

(use-package cov)


;;================================================================================
;; Spell checking
;;================================================================================

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

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
	'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

(when ispell-program-name
  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
  ;; Hack ispell-local-dictionary-alist instead.
  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
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
  (add-hook 'text-mode-hook 'text-mode-hook-setup)

  (use-package flycheck-aspell :ensure t
    :config
    (advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
    (defun flycheck-maybe-recheck (_)
      (when (bound-and-true-p flycheck-mode)
	(flycheck-buffer)))
    )
  )

(use-package unicode-fonts)

;; http://unifoundry.com/pub/unifont/unifont-14.0.01/font-builds/unifont-14.0.01.ttf
(defvar kb/fonts '((:url "http://unifoundry.com/pub/unifont/unifont-14.0.01/font-builds/"
			 :fonts "unifont-14.0.01.ttf"
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

(use-package undo-tree
  :delight ""
  :config (global-undo-tree-mode))

(use-package beacon
  :ensure t
  :pin melpa
  :config (beacon-mode 1)
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
    (set-face-attribute (car face) nil :font "Hack" :weight 'regular :height (cdr face)))
  

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
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package gnuplot)

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :custom
  (org-ellipsis " ▾")
  (org-hide-leading-stars t)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-src-fontify-natively t)
  (org-export-with-smart-quotes t)
  (org-html-postamble nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  :config
  (defun kb/org-mode-setup ()
    (org-indent-mode)
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
  (kb/org-font-setup)

  :hook (org-mode . kb/org-mode-setup)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(use-package htmlize)

(use-package ox-reveal
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-mathjax t)
  )

(use-package org-journal
  :custom (org-journal-dir "~/projects/journal/")
  )
(use-package deft
  :custom
  (deft-directory org-journal-dir)
  (deft-recursive t)
  )

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


(use-package pabbrev :delight)

(use-package iedit
  :bind ("C-c ;" . iedit-mode))

(use-package volatile-highlights
  :delight
  :after undo-tree
  :custom
  (Vhl/highlight-zero-width-ranges t)
  :config (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

(use-package ws-butler
  :ensure t
  :hook
  ((c-mode-common text-mode fundamental-mode) . ws-butler-mode))

(use-package cycle-quotes
  :bind ("C-c q" . cycle-quotes))

(use-package bool-flip
  :bind ("C-c C-b" . bool-flip-do-flip))

(use-package amx)
(use-package company
  :hook (after-init . kb/company-hook)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-clang-excecutable "clang-11")
  :config
  (defun kb/company-hook ()
    "Hook to setup company mode"
    (global-company-mode)
    (message "KB/company-hook"))
  (delete 'company-semantic company-backends)
  (delete 'company-oddmuse company-backends)
  (delete 'company-gtags company-backends)
  (if (fboundp 'yas-expand)
      (add-hook 'c-mode-common (lambda ()
                                 (message "Yo this is yasnippet backend")
                                 (add-to-list (make-local-variable 'company-backends) 'company-yasnippet))))

  :bind
  ;; ((:map c-mode-map ("TAB" . company-complete))
  ;;  (:map c++-mode-map ("TAB" . company-complete)))
  (:map global-map ([remap dabbrev-expand] . company-dabbrev))
  (:map company-active-map ("<tab>" . company-complete-selection))
  ;;(:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  )

(use-package company-statistics
  :config
  (company-statistics-mode))
(use-package company-posframe
  :disabled
  ;; :hook (company-mode . company-posframe-mode-hook)
  :config
  ;; (require 'company-posframe)
  (with-eval-after-load 'company (company-posframe-mode 1))
  )
(use-package company-quickhelp
  :disabled
  :config
  (with-eval-after-load 'company 
    (company-quickhelp-mode)
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
  )

(use-package company-c-headers
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
  :config
  (company-ctags-auto-setup))

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode)
  )
(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode #'highlight-defined-mode)
  )

(use-package highlight-operators
  :hook (c-common-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :pin "melpa"
  :config (hes-mode))

;;(use-package paren-face
;;   :ensure t
;;   :init (global-paren-face-mode))

(when (executable-find "clang-format")
  (use-package clang-format
    :bind
    (:map global-map ("C-M-'" . clang-format-region)))
  (use-package clang-format+
    :ensure t
    :custom (clang-format+-context 'modification)
    ;;:hook (c-mode-common . clang-format+-mode)
    )
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred which-key)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :custom
  ;; (lsp-print-performance t)
  ;; (lsp-enable-xref t)
  (lsp-log-io nil)
  (lsp-idle-delay 1.0)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols project))

  :hook
  ((c++-mode . lsp-deferred)
   (c-mode . lsp-deferred))
  ;; :bind (:map lsp-mode-map ("M-." . lsp-find-declaration))
  :config
  (with-eval-after-load 'lsp-mode
    '(define-key lsp-mode-map (kbd "<tab>") 'company-indent-or-complete-common))
  (if (package-installed-p 'which-key)
      (lsp-enable-which-key-integration t))
  
  (setq lsp-enable-snippet nil)
  (with-eval-after-load 'yasnippet
    (setq lsp-enable-snippet t))
  
  (defun kb/lsp-breadcrumb-face-setup ()
    "Fix headerlime colors for breadcrumbs"
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil :foreground "yellow" :background nil   :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil  :foreground "PaleGreen" :background nil :width 'extra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-separator-face nil :foreground "green" :background nil :weight 'ultra-bold :width 'ultra-condensed)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil :foreground "green" :background nil :weight 'light :width 'ultra-condensed)
    (set-face-background 'header-line "black")
    )
  (add-hook 'lsp-headerline-breadcrumb-mode-hook 'kb/lsp-breadcrumb-face-setup)
  )

;; (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay 1)
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-peek-enable t
	lsp-ui-sideline-enable t
	lsp-ui-imenu-enable t
	;; lsp-ui-flycheck-enable t
	)
  )
(use-package lsp-ivy
  :ensure t
  :commands
  (lsp-ivy-workspace-symbol)
  )
(use-package treemacs
  :bind
  ([f8] . treemacs)
  :custom-face
  ;; update font size
  (treemacs-directory-face ((t (:inherit font-lock-function-name-face :height 0.8))))
  (treemacs-file-face ((t (:inherit default :height 0.9))))
  (treemacs-root-face ((t (:inherit font-lock-constant :height 1.1))))
  (treemacs-root-unreadable-face ((t (:inherit treemacs-root-face :height 0.9))))
  (treemacs-root-remote-face ((t (:inherit font-lock-function-name-face :height 2.0))))
  (treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :height 0.9))))
  (treemacs-git-ignored-face ((t (:inherit font-lock-comment-face :height 0.9))))
  (treemacs-git-untracked-face ((t (:inherit font-lock-string-face :height 0.9))))
  )
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package ztree
  :pin elpa)

(use-package which-key
  :defer 0
  :delight which-key-mode
  :custom (which-key-idle-delay 1)
  :config (which-key-mode)
  (add-to-list 'which-key-replacement-alist
	       '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))

  (add-to-list 'which-key-replacement-alist
	       '((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "h/\\1")))
  (which-key-setup-side-window-right)
  )

(use-package which-key-posframe
  :config (which-key-posframe-mode))

(use-package meson-mode
  :hook (meson-mode . company-mode))
(use-package ninja-mode)
(use-package yaml-mode)

(use-package ietf-docs
  :bind
  (:map global-map
	("C-c k o" . ietf-docs-open-at-point)))
(use-package rfc-mode)
(use-package yang-mode
  :config
  (setq blink-matching-paren-distance nil)
  (defconst kb/yang-style
    '("BSD"
      (c-basic-offset . 2))
    "Style used for YANG source editing.")

  (defun kb/yang-mode-hook ()
    "My style used while editing YANG sources."
    (c-add-style "kb/yang-style" kb/yang-style t)
    (fill-column . 100)
    (font-lock-maximum-decoration . t)
    (font-lock-mode . t)
    (indent-tabs-mode . nil))
  (add-hook 'yang-mode-hook 'kb/yang-mode-hook)

  (defun show-onelevel ()
    "show entry and children in outline mode"
    (interactive)
    (outline-show-entry)
    (outline-show-children))

  (defun my-outline-bindings ()
    "sets shortcut bindings for outline minor mode"
    (interactive)
    (local-set-key [?\C-,] 'hide-body)
    (local-set-key [?\C-.] 'show-all)
    (local-set-key [C-up] 'outline-previous-visible-heading)
    (local-set-key [C-down] 'outline-next-visible-heading)
    (local-set-key [C-left] 'hide-subtree)
    (local-set-key [C-right] 'show-onelevel)
    (local-set-key [M-up] 'outline-backward-same-level)
    (local-set-key [M-down] 'outline-forward-same-level)
    (local-set-key [M-left] 'hide-subtree)
    (local-set-key [M-right] 'show-subtree))

  (add-hook
   'outline-minor-mode-hook
   'my-outline-bindings)

  (defconst sort-of-yang-identifier-regexp "[-a-zA-Z0-9_\\.:]*")

  :hook
  (yang-mode . (lambda ()
		 (outline-minor-mode)
		 (setq outline-regexp
		       (concat "^ *" sort-of-yang-identifier-regexp " *"
			       sort-of-yang-identifier-regexp
			       " *{"))))
  )
(use-package flycheck-yang
  :hook (yang-mode . (lambda () flycheck-mode))
  )
(use-package protobuf-mode :ensure t
  :config
  (defconst kb/protobuf-style
    '("linux"
      (c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode . (lambda () (c-add-style "kb/protobuf" kb/protobuf-style t)))
  )

(when (package-installed-p 'all-the-icons)
  (use-package all-the-icons
    :config
    ;; (all-the-icons-install-fonts)
    )
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))
  (use-package all-the-icons-ibuffer
    :config
    (with-eval-after-load 'ibuffer
      '(all-the-icons-ibuffer-mode 1)))
  (use-package treemacs-all-the-icons)
  (use-package treemacs-icons-dired)
  (use-package all-the-icons-ivy
    :config
    (all-the-icons-ivy-setup))
  (use-package all-the-icons-ivy-rich
    :config (all-the-icons-ivy-rich-mode 1))
  (use-package all-the-icons-completion
    :config
    (all-the-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
  )

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  ;; THe :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode
  ;; gets enabled right away. Note that this forces loading the
  ;; package
  (marginalia-mode))

(use-package hydra
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
(use-package use-package-hydra)

(use-package hercules)

(use-package counsel
  :custom
  (counsel-file-file-at-point t)
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  :config
  (counsel-mode)
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
	 ;; ("C-c t" . counsel-load-theme)
	 ("C-c C-o" . counsel-imenu)
	 ([remap insert-register] . counsel-register)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  )

(use-package counsel-projectile
  :config(counsel-projectile-mode))
(use-package counsel-ag-popup)
(use-package counsel-edit-mode
  :config (counsel-edit-mode-setup-ivy))
(when (executable-find "jq")
  (use-package counsel-jq)
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-describe-symbol-function #'helpful-symbol)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package swiper
  :ensure t
  :bind ([remap isearch-forward] . swiper-isearch)
  )

(use-package ivy
  :delight
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

  :bind (("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 ("C-c m" . kb/ivy-switch-project)
	 ("C-c n" . kb/ivy-switch-git)
	 )
  :custom
  (ivy-use-virutal-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  )
(use-package ivy-posframe
  :defines
  (ivy-posframe-display-functions-alist)
  :config
  ;; display at `ivy-posframe-style'
  (defun ivy-posframe-display-at-frame-top-right (str)
    (ivy-posframe--display str #'posframe-poshandler-frame-top-right-corner))

  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-right)
  ;; 					       (t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-height-alist '((swiper-isearch	.	10)
				    (swiper		.	10)
                                    (t			.	40)))
  (setq ivy-posframe-display-functions-alist
	'((swiper			.	ivy-display-function-fallback)
          (complete-symbol		.	ivy-posframe-display-at-point)
          (counsel-M-x			.	ivy-posframe-display-at-window-bottom-left)
	  (ivy-switch-buffer		.	ivy-posframe-display-at-window-center)
	  (counsel-find-file		.	ivy-posframe-display-at-window-bottom-left)
	  (counsel-describe-variable	.	ivy-posframe-display-at-frame-top-right)
	  (t				.	ivy-posframe-display-at-frame-top-right)
          ;; (t               . ivy-posframe-display)
	  ))
  (setq ivy-posframe-parameters
	'((left-fringe . 8)
          (right-fringe . 8)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  (ivy-posframe-mode 1)
  )

(use-package ivy-xref
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package ivy-avy)
(use-package ivy-hydra)

(use-package ivy-emoji
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

(use-package fuz
  :config
  (require 'fuz)
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  )
(use-package ivy-fuz)

(use-package smart-compile)

;; load generic modes which support e.g. batch files
;;(use-package generic-x)

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Use silversearcher-ag if available.
(if (executable-find "ag")
    (progn
      (use-package ag)
      (use-package wgrep-ag)
      )
  (warn "silversearcher-ag not found")
  )

;; :delight '(:eval (concat " " (projectile-project-name)))
(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  :config
  (message "Running projectile mode")
  (projectile-mode)
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects))
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

(use-package treemacs-projectile)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :custom
  (python-shell-interpreter (executable-find "python3"))
  (py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  (py-force-py-shell-name-p t)
  (py-shell-switch-buffers-on-execute-p t)
  (py-switch-buffers-on-execute-p t)
  (py-split-windows-on-execute-p nil)
  (py-smart-indentation t))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp)))
  :init (setq lsp-python-ms-auto-install-server t))

(use-package magit
  :commands magit-status
  ;; :bind (:map global-map
  ;; 	      ("C-c n". magit-list-repos))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("~/projects" . 2)
				  ("~/projects/mavenir-dev/files/subprojects" . 2)))
  :config 
  (setq git-commit-summary-max-length 50)
  )

(use-package git-timemachine)

(use-package transient-posframe
  :config
  (transient-posframe-mode))
(use-package treemacs-magit)
;;--------------------------------------------------------------------------------
;; Optional packages
;;--------------------------------------------------------------------------------
(if (not (package-installed-p 'smartparens))
    (progn
      "smartparens not installed: Using builtin paren mode."
      (setq show-paren-style 'parenthesis)
      (setq show-paren-when-point-inside-paren t)
      (setq show-paren-when-point-in-periphery t)
      (show-paren-mode 1))

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

(use-package ox-jira)

(if (executable-find "dot")
    (use-package graphviz-dot-mode
      :custom
      (when (executable-find "xdot")
	(graphviz-dot-view-command "xdot %s"))
      :config
      (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))
  "dot command not found in system"
  )

(when (package-installed-p 'yasnippet)
  (use-package yasnippet
    :after hydra
    :defines (yas-snippet-dirs yas-active-snippets)
    :pin melpa
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
    :bind ((:map yas-keymap
		 ("<return>" . yas-exit-all-snippets)
		 ("C-e" . yas/goto-end-of-active-field)
		 ("C-a" . yas/goto-start-of-active-field))
	   (:map yas-minor-mode-map ("<f2>" . hydra-yas/body)))
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
    )
  (use-package yasnippet-snippets)
  (use-package yasnippet-classic-snippets)
  (use-package ivy-yasnippet)
  )

(use-package plantuml-mode
  :custom
  (plantuml-jar-path (expand-file-name "~/.java/libs/plantuml-nodot.1.2021.12.jar"))
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4)
  :hook (plantuml-mode . (lambda ()
			   (set-fill-column 100)
			   (display-fill-column-indicator-mode)
			   ))
  :bind (:map plantuml-mode-map
	      ("C-c C-p" . plantuml-preview-buffer))
  :config
  (plantuml-set-output-type "svg")
  )
(use-package flycheck-plantuml
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-plantuml)
    (flycheck-plantuml-setup)))

(message "Init finished")
;;; init.el ends here
(put 'magit-clean 'disabled nil)
