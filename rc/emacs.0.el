;; -*- mode: lisp; coding: utf-8-unix -*-

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

;; --[ Loading Libraries of Lisp Code for Emacs ]------------------------

;; make loaded files give a message
(defadvice load (before debug-log activate)
  (message "Loading %s..." (locate-library (ad-get-arg 0))))

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
(add-to-list 'load-path
             (concat my-site-lisp-directory "doxymacs"))
(if (version<= emacs-version "21")
    (add-to-list 'load-path
                 (concat my-site-lisp-directory "url"))
  )
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

;;; ----[ 19.11 (info "(emacs)Useless Whitespace")

;;; ----[ 19.17 Cursor Display

;; see what I'm typing *immediately*
(setq echo-keystrokes 0.01)

;;; ----[ Temporary Displays

;; make the help, apropos and completion windows the right height for their
;; contents
(temp-buffer-resize-mode t)

;;; ----[ 23.18 (info "(emacs)File Conveniences")

;; show image files as images (not as semi-random bits)
(auto-image-file-mode 1)

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

;;; ----[ 26.20 (info "(emacs)Mouse Avoidance")

;; make mouse pointer stay out of the way of editing
(when window-system
  (require 'avoid)
  (mouse-avoidance-mode 'animate))

;; mouse wheel support
(mwheel-install)

; last --------------------------------------------------------------------------------
;; use M-{up,right,down,left} for windmove
(if (version<= emacs-version "21")
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
;; aspell mode
;;--------------------------------------------------------------------------------
(if running-ms-windows
    ;(setq ispell-program-name "d:/karol/cygwin/bin/aspell.exe")
    (message "not using aspell under ms windows")
  (setq ispell-program-name "aspell")
  )

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
;; Add missing support functions
;;--------------------------------------------------------------------------------
(if (version< emacs-version "24")
    (progn
      (defun utf-16-le-pre-write-conversion (start end) nil)
      (defun utf-16-le-pre-write-conversion (beg end)
        "Semi-dummy pre-write function effectively to autoload ucs-tables."
        ;; Ensure translation table is loaded, if available.
        (require 'ucs-tables nil t)
        ;; Don't do this again.
        (coding-system-put 'mule-utf-16-le 'pre-write-conversion nil)
        nil)
      (defun utf-16-be-pre-write-conversion (start end) nil)
      )
  "Emacs >= 24 should have these functions defined"
  )
