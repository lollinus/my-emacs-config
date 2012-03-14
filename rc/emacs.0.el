;; -*- mode: lisp; coding: utf-8-unix -*-
;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "karol.barski@tieto.com")


;; Emacs type -- are we running XEmacs (or GNU Emacs)?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(if running-xemacs
    ;; don't offer migration of the init file
    (setq load-home-init-file t))

;; --[ Variables ]-------------------------------------------------------
;; limit on number of Lisp variable bindings & unwind-protects
;; (30 times its default value)
;; (setq max-specpdl-size (* 30 1000))

;; limit serving to catch infinite recursions for you before they
;; cause actual stack overflow in C, which would be fatal for Emacs
;; (setq max-lisp-eval-depth (* 100 300))

;; taken from emacs-23.xxx
(defconst user-emacs-directory
  (if (eq system-type 'ms-dos)
      ;; MS-DOS cannot have initial dot.
      "~/_emacs.d/"
    "~/.emacs.d/")
  "Directory beneath which additional per-user Emacs-specific files are placed.
 Various programs in Emacs store information in this directory.
 Note that this should end with a directory separator.")

;; --[ Macros ]----------------------------------------------------------

(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (not running-xemacs) (cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if running-xemacs (cons 'progn body)))
;; --[ Loading Libraries of Lisp Code for Emacs ]------------------------

;; make loaded files give a message
(GNUEmacs
    (defadvice load (before debug-log activate)
      (message "Loading %s..." (locate-library (ad-get-arg 0)))))

;;; ----[ Features

;; (defvar missing-packages-list nil
;;   "List of packages that `try-require' can't find.")

;; ;; attempt to load a feature/library, failing silently
;; (defun try-require (feature)
;;   "Attempt to load a library or module. Return true if the
;; library given as argument is successfully loaded. If not, instead
;; of an error, just add the package to a list of missing packages."
;;   (condition-case err
;;       ;; protected form
;;       (progn
;;         (message "Checking for library `%s'..." feature)
;;         (if (stringp feature)
;;             (load-library feature)
;;           (require feature))
;;         (message "Checking for library `%s'... Found" feature))
;;     ;; error handler
;;     (file-error  ; condition
;;      (progn
;;        (message "Checking for library `%s'... Missing" feature)
;;        (add-to-list 'missing-packages-list feature))
;;      nil)))


;;; ----[ Library Search

;; The most important directories are the last!

;; `local-site-lisp-directory' is there so that you have an easy way of
;; installing your own (possibly not distro packaged) Emacs add-ons which
;; are specific to the version of Emacs your running. This keeps your local
;; add-ons apart from distro supplied ones. If your have a `/usr/local'
;; partition, it also means you can do a complete re-install of Emacs (or
;; even your Linux distro) without impacting on stuff you have added by
;; hand.

;; 3.
(if running-ms-windows
    (defvar my-site-lisp-directory (concat (getenv "HOME") "\\site-lisp\\")
      "Name of directory where my personal additional Emacs Lisp files reside.")
  (defvar my-site-lisp-directory "~/site-lisp/"
    "Name of directory where my personal additional Emacs Lisp files reside.")
  )

(add-to-list 'load-path my-site-lisp-directory)
(if (string-equal "21" (substring emacs-version 0 2))
    (add-to-list 'load-path
                 (concat my-site-lisp-directory "cua")
                 )
  )
(add-to-list 'load-path
             (concat my-site-lisp-directory "doxymacs"))
;(add-to-list 'load-path
;        (concat my-site-lisp-directory "tempo"))
(if (string-equal "21" (substring emacs-version 0 2))
    (add-to-list 'load-path
                 (concat my-site-lisp-directory "url"))
  )
; turn off clearcase for emacs 24 because of config error
; (if (not running-ms-windows)
;   (add-to-list 'load-path
;          (concat my-site-lisp-directory "clearcase"))
;   )
(add-to-list 'load-path
             (concat my-site-lisp-directory "psvn"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "ruby-mode"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "git"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "chrome-edit"))

;; don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)

;;; ----[ 8.8 Continuation Lines

;; (setq default-truncate-lines nil)
;; (setq truncate-partial-width-windows nil)

(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)))


;; --[ 9 The (info "(emacs)Minibuffer") ]--------------------------------

(message "9 The Minibuffer...")

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
;;(GNUEmacs
;;    (file-name-shadow-mode 1))

;; minibuffer window expands vertically as necessary to hold the text that
;; you put in the minibuffer
(setq resize-mini-windows t)

;; minibuffer completion incremental feedback
(GNUEmacs
    (icomplete-mode))

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)


;;; ----[ 19.11 (info "(emacs)Useless Whitespace")

;; highlight trailing whitespaces
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace t))))
      '(text-mode-hook
        c-mode-hook
        c++-mode-hook
        emacs-lisp-mode-hook
        java-mode-hook
        python-mode-hook
        ruby-mode-hook
        shell-script-mode-hook))

;; ;; highlight tabs
;; (when (try-require 'show-wspace)
;;   (add-hook 'font-lock-mode-hook 'highlight-tabs));

;; colorize number too (like constant)
(defun font-lock-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
                          '(("[^a-zA-Z_]\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face) ; hexa
                            ("[^a-zA-Z_]\\(-?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face) ; float
                            ("[^a-zA-Z_1-9]\\(-?[0-9]+L?\\)" 1 font-lock-constant-face)))) ; int
(add-hook 'php-mode-hook 'font-lock-fontify-numbers)
(add-hook 'c-mode-hook 'font-lock-fontify-numbers)
(add-hook 'c++-mode-hook 'font-lock-fontify-numbers)
(add-hook 'perl-mode-hook 'font-lock-fontify-numbers)
(add-hook 'ruby-mode-hook 'font-lock-fontify-numbers)
(add-hook 'python-mode-hook 'font-lock-fontify-numbers)
(add-hook 'css-mode-hook 'font-lock-fontify-numbers)
(add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-numbers)
(add-hook 'js2-mode-hook 'font-lock-fontify-numbers)

(setq inhibit-splash-screen t)

;; delete all the trailing whitespaces and tabs across the current buffer
(defun my-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  )

(global-set-key (kbd "C-c t") 'my-delete-trailing-whitespaces-and-untabify)

;;; ----[ 19.15 The (info "(emacs)Cursor Display")

(GNUEmacs
    ;; using cursor color to indicate some modes (read-only, insert and
    ;; overwrite modes)
    (setq my-set-cursor-color-color "")
    (setq my-set-cursor-color-buffer "")

    (defun my-set-cursor-color-according-to-mode ()
      "Change cursor color according to some minor modes."
      (let ((color
             (if buffer-read-only "purple1"
               (if overwrite-mode "red"
                 "rgb:15/FF/00"))))  ;; insert mode
        (unless (and (string= color my-set-cursor-color-color)
                     (string= (buffer-name) my-set-cursor-color-buffer))
          (set-cursor-color (setq my-set-cursor-color-color color))
          (setq my-set-cursor-color-buffer (buffer-name)))))

    (add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode))

;;; ----[ 19.17 Cursor Display

;; see what I'm typing *immediately*
(setq echo-keystrokes 0.01)

;;; ----[ Temporary Displays

;; make the help, apropos and completion windows the right height for their
;; contents
(GNUEmacs
    (temp-buffer-resize-mode t))

;;; ----[ 23.18 (info "(emacs)File Conveniences")

;; show image files as images (not as semi-random bits)
(GNUEmacs
    (auto-image-file-mode 1))

;; find file (or URL) at point
(require 'ffap)

;; don't use default key bindings, as I want some of them to be defined
;; differently (C-x C-r, for example)
;; visit a file
(global-set-key (kbd "<f3>") 'find-file-at-point)

;;; ----[ 26.9 Making and Using a (info "(emacs)Speedbar") Frame

;; everything browser (into individual source files), or Dired on steroids
;; (when (try-require 'speedbar)
(require 'speedbar)
;; number of spaces used for indentation
(setq speedbar-indentation-width 2)
;; expand/collapse latex sections
(speedbar-add-supported-extension '(".tex" ".bib" ".w"))
;; jump to speedbar frame
(global-set-key (kbd "<f4>") 'speedbar-get-focus)

;; bind the arrow keys in the speedbar tree
;; [http://www.uweb.ucsb.edu/~dreamtheorist/emacs.html]
(define-key speedbar-key-map [(right)] 'speedbar-expand-line)
(define-key speedbar-key-map [(left)] 'speedbar-contract-line)

;;; ----[ 26.16 (info "(emacs)Menu Bars")

;; turn menus off
(unless window-system
    (menu-bar-mode 0))

;;; ----[ 26.20 (info "(emacs)Mouse Avoidance")

;; make mouse pointer stay out of the way of editing
(when window-system
  (require 'avoid)
  (mouse-avoidance-mode 'animate))

;; mouse wheel support
(GNUEmacs
    (mwheel-install))

; last --------------------------------------------------------------------------------
;; use M-{up,right,down,left} for windmove
(if (not (string-equal "21" (substring emacs-version 0 2)))
    (windmove-default-keybindings 'meta)
  )

;; C-x k == C-x # when editing emacsclient is waiting
;; (add-hook 'server-switch-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-x k") '(lambda ()
;;                                     (interactive)
;;                                     (if server-buffer-clients
;;                                         (server-edit)
;;                                       (ido-kill-buffer))))))

;; --[ 28 (info "(emacs)Major Modes") ]----------------------------------

(message "28 Major Modes...")

(autoload 'sql-mode "sql" nil)

;; list of filename patterns
;; vs. corresponding major mode functions
(setq auto-mode-alist
      (append '(
                ("\\.txt$"                             . indented-text-mode)
                                                         ;; or text-mode
                ("\\.css\\'"                           . css-mode)
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
                ("\\.\\(htm\\(l\\)?\\|xhtml\\)$"       . xml-mode)
                ("\\.\\(svg\\|xml\\|xsl\\)\\'"         . xml-mode)
;;                ("\\.\\(htm\\(l\\)?\\|xhtml\\)$"       . nxhtml-mode)
;;                ("\\.\\(svg\\|xml\\|xsl\\)\\'"         . nxml-mode)
                ("\\.tex$"                             . LaTeX-mode)
                ("[mM]akefile"                         . makefile-mode)
                ("\\.cgi$"                             . perl-mode)
                ("\\.bash$"                            . shell-script-mode)
                ("\\.sql$"                             . sql-mode)
                ("\\.expect$"                          . tcl-mode)
                ("\\.js$"                              . java-mode)
                ("\\.dcl$"                             . dtd-mode)
                ("\\.dec$"                             . dtd-mode)
                ("\\.dtd$"                             . dtd-mode)
                ("\\.ele$"                             . dtd-mode)
                ("\\.ent$"                             . dtd-mode)
                ("\\.mod$"                             . dtd-mode)
                ("\\.owl$"                             . owl-mode)
                (".ssh/config\\'"                      . ssh-config-mode)
                ("sshd?_config\\'"                     . ssh-config-mode)
                ) auto-mode-alist))

;; load generic modes which support e.g. batch files
(require 'generic-x)

;;; ----[ 31.8 Completion for Symbol Names

;; dynamic word-completion code
(require 'completion)  ;; a.o., add completion-c-mode-hook
;; load the default completions file
(initialize-completions)
(global-set-key [(control tab)] 'complete)

;;--------------------------------------------------------------------------------
;; My customized emacs
;;--------------------------------------------------------------------------------
;; scroll bar on the right side
(if window-system
    (set-scroll-bar-mode 'right)
  )

;; fancy streching cursor
(setq x-stretch-cursor t)
(global-hl-line-mode t)

;; show column number in mode-line
(column-number-mode t)
(line-number-mode t)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)
(tool-bar-mode 0) ; disable toolbar
(setq kill-whole-line t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format "Emacs: %b %+%+ %f ")

;; code for including abbreviated file paths in mode line
;; (GNUEmacs
;;     (require 'mode-line)
;;     (mode-line-toggle-display nil))

;;--------------------------------------------------------------------------------
;; buffer switch
;;--------------------------------------------------------------------------------
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "<f9>") 'bs-cycle-previous)
(global-set-key (kbd "<f10>")  'bs-cycle-next)

;; fix buffer-killing for bs + gnuclient
;; (eval-after-load "gnuclient"
;;   '(fset 'kill-buffer 'my-server-kill-buffer))

;; (defun my-server-kill-buffer (buffer)
;;   "Call `server-kill-buffer' but make sure to return the correct value."
;;   (interactive "bKill buffer ")
;;   (server-kill-buffer (get-buffer buffer))
;;   (not (buffer-name (get-buffer buffer))))

;; If makefile doesn't exist compile with g++ -Wall -o <current file name> <current file name>
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "g++")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

;;--------------------------------------------------------------------------------
;; iswitchb-mode for interactive switch buffers
;;--------------------------------------------------------------------------------
(iswitchb-mode 1)

;;----------------------------------------------------------------------
;; regexp higlight
;;----------------------------------------------------------------------
(require 'highlight-regexp)

;;--------------------------------------------------------------------------------
;; time stamp writing in edited files
;; add Time-stamp: <> or Time-stamp: "" so emacs can write timestamp there
;;--------------------------------------------------------------------------------
;; (add-hook 'write-file-hooks 'time-stamp)

;;--------------------------------------------------------------------------------
;; aspell mode
;;--------------------------------------------------------------------------------
(if running-ms-windows
    ;(setq ispell-program-name "d:/karol/cygwin/bin/aspell.exe")
    (message "not using aspell under ms windows")
  (setq ispell-program-name "aspell")
  )

;;--------------------------------------------------------------------------------
;; Graphviz dot mode
;;--------------------------------------------------------------------------------
(load "graphviz-dot-mode.el" nil t t)
(add-to-list 'auto-mode-alist
  '("\\.gv$" . graphviz-dot-mode))

;;(load "auctex.el" nil t t)

;;--------------------------------------------------------------------------------
;; clipboard is encoded utf-16
;;--------------------------------------------------------------------------------
;; (if running-ms-windows
;;     (progn
;;       (set-selection-coding-system 'utf-16-le-dos)
;;       (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;;       )
;;   )

;;--------------------------------------------------------------------------------
;; Detect endianness of UTF-16 containing Byte Order Mark U+FEFF
;;--------------------------------------------------------------------------------
;; (add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-le) t)

;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
;; Detect EOL mode by looking for CR/LF on the first line
;; (add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

;;--------------------------------------------------------------------------------
;; table edition mode
;;--------------------------------------------------------------------------------
;; (require 'table)

;;--------------------------------------------------------------------------------
;; Add missing support functions
;;--------------------------------------------------------------------------------
(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-le-pre-write-conversion (beg end)
  "Semi-dummy pre-write function effectively to autoload ucs-tables."
  ;; Ensure translation table is loaded, if available.
  (require 'ucs-tables nil t)
  ;; Don't do this again.
  (coding-system-put 'mule-utf-16-le 'pre-write-conversion nil)
  nil)
(defun utf-16-be-pre-write-conversion (start end) nil)


;;--------------------------------------------------------------------------------
;; doxygen mode
;;--------------------------------------------------------------------------------
;;(require 'doxygen)
;;(setq doxymacs-doxygen-dirs "d:/Karol/Programy/doxygen/bin")
(require 'doxymacs)
(setq doxymacs-use-external-xml-parser t)
(if running-ms-windows
    (progn
      (setq doxymacs-external-xml-parser-executable "xmllint.exe")
      )
  (setq doxymacs-external-xml-parser-executable "xmllint")
  )
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "C++!")
