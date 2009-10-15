;; -*- mode: lisp; coding: utf-8 -*-
;;
(setq user-full-name "Karol Barski")
(setq user-mail-address "lollinus@gmail.com")

;; OS type -- are we running Microsoft Windows?
(defvar running-ms-windows (eq system-type 'windows-nt))

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
    (defvar my-site-lisp-directory "d:/karol/programy/site-lisp/"
      "Name of directory where my personal additional Emacs Lisp files reside.")
  (defvar my-site-lisp-directory "~/site-lisp/"
    "Name of directory where my personal additional Emacs Lisp files reside.")
  )

(add-to-list 'load-path my-site-lisp-directory)
(add-to-list 'load-path
             (if (string-equal "21" (substring emacs-version 0 2))
                 (progn 
                   ;; emacs 21 specific plugins
                   (concat my-site-lisp-directory "linum/emacs21")
                   (concat my-site-lisp-directory "cua")
                 )
               (concat my-site-lisp-directory "linum/emacs22")
               )
             )
(add-to-list 'load-path
         (concat my-site-lisp-directory "folding"))
(add-to-list 'load-path
         (concat my-site-lisp-directory "color-theme-library"))
(add-to-list 'load-path
         (concat my-site-lisp-directory "doxymacs"))
;(add-to-list 'load-path
;        (concat my-site-lisp-directory "tempo"))
(if (string-equal "21" (substring emacs-version 0 2))
    (add-to-list 'load-path
                 (concat my-site-lisp-directory "url"))
  )
(add-to-list 'load-path
         (concat my-site-lisp-directory "clearcase"))
(add-to-list 'load-path
         (concat my-site-lisp-directory "psvn"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "ruby-mode"))
(add-to-list 'load-path
             (concat my-site-lisp-directory "git"))

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

;;----------------------------------------------------------------------
;; cedet configuration load
(load "~/.emacs-rc-cedet.el")

;;----------------------------------------------------------------------
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
(add-hook 'css-mode-hook 'font-lock-fontify-numbers)
(add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-numbers)
;; (add-hook 'js2-mode-hook 'font-lock-fontify-numbers)

(setq inhibit-splash-screen t)

;; delete all the trailing whitespaces and tabs across the current buffer
(defun my-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  )

(global-set-key [(control c) (t)] 'my-delete-trailing-whitespaces-and-untabify)

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


(require 'column-marker)
;; highlight column 80 in foo mode
;; (add-hook emacs-lisp-mode-hook
;;    (lambda ()
;;      (interactive)
;;      (column-marker-1 80)))

;;   ;; use `C-c m' interactively to highlight with `column-marker-1-face'
;; (global-set-key [(control c) (m)] 'column-marker-1)

;; ;; conflicts with the above!
;; (when (try-require 'wide-column)
;;     (setq wide-column-start-width 60)
;;     (setq wide-column-default-cursor-colour "rgb:15/FF/00"))
;; ;;    (add-hook 'text-mode-hook 'wide-column-mode)


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
(global-set-key [(f3)] 'find-file-at-point)

;;; ----[ 26.9 Making and Using a (info "(emacs)Speedbar") Frame

;; everything browser (into individual source files), or Dired on steroids
;; (when (try-require 'speedbar)
(require 'speedbar)
;; number of spaces used for indentation
(setq speedbar-indentation-width 2)
;; expand/collapse latex sections
(speedbar-add-supported-extension '(".tex" ".bib" ".w"))
;; jump to speedbar frame
(global-set-key [(f4)] 'speedbar-get-focus)

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
(set-scroll-bar-mode 'right)

;; fancy streching cursor
(setq x-stretch-cursor t)

;; show column number in mode-line
(column-number-mode t)
(line-number-mode t)
;; line numbering with linum (WTF it's not activated by defaults?!)
(require 'linum)
;(global-linum-mode t)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)
(tool-bar-mode nil) ; disable toolbar
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


;; --[ Folding mode ]----------------------------------------------------
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)

;;--------------------------------------------------------------------------------
;; buffer switch
;;--------------------------------------------------------------------------------
(require 'bs)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key [(f9)]   'bs-cycle-previous)
(global-set-key [(f10)]  'bs-cycle-next)

;; fix buffer-killing for bs + gnuclient
(eval-after-load "gnuclient"
  '(fset 'kill-buffer 'my-server-kill-buffer))

(defun my-server-kill-buffer (buffer)
  "Call `server-kill-buffer' but make sure to return the correct value."
  (interactive "bKill buffer ")
  (server-kill-buffer (get-buffer buffer))
  (not (buffer-name (get-buffer buffer))))

;; If makefile doesn't exist compile with g++ -Wall -o <current file name> <current file name>
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "g++ -Wall -o "
                           (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                           " "
                           (file-name-nondirectory (buffer-file-name)))))))

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
;; (if running-ms-windows
;;     (setq ispell-program-name "d:/karol/cygwin/bin/aspell.exe")
;;   (setq  ispell-program-name "aspell")
;;   )

;;--------------------------------------------------------------------------------
;; Graphviz dot mode
;;--------------------------------------------------------------------------------
(load "graphviz-dot-mode.el" nil t t)
(add-to-list 'auto-mode-alist
  '("\\.gv$" . graphviz-dot-mode))

;;(load "auctex.el" nil t t)

;;--------------------------------------------------------------------------------
;; regional settings
;;--------------------------------------------------------------------------------
;;(set-default-coding-systems     'iso-latin-2)
;;(set-keyboard-coding-system           'iso-latin-2)
;;(prefer-coding-system                 'iso-latin-2-unix)

;;(set-language-environment               'Polish)
;; (set-language-environment               'utf-8)
;; (set-default-coding-systems             'utf-8)
;;(setq file-name-coding-system           'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
;;(set-keyboard-coding-system             'utf-8)
;; (set-terminal-coding-system             'utf-8)
;; (set-clipboard-coding-system            'utf-8)
;; (set-selection-coding-system            'utf-8)
(prefer-coding-system                   'iso-latin-2-unix)


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
;; color themes
;;--------------------------------------------------------------------------------
(require 'color-theme)
;;(load-library "color-theme-library")
;;(setq color-theme-is-global t)
(color-theme-initialize)
;;(color-theme-classic)

(if window-system
    (color-theme-subtle-hacker)
  (color-theme-hober))

;;--------------------------------------------------------------------------------
;; doxygen mode
;;--------------------------------------------------------------------------------
;;(require 'doxygen)
(require 'doxymacs)
(setq doxymacs-use-external-xml-parser t)
(if running-ms-windows
    (progn
      (setq doxymacs-doxygen-dirs "d:/Karol/Programy/doxygen/bin")
      (setq doxymacs-external-xml-parser-executable "xmllint.exe")
      )
  (setq doxymacs-external-xml-parser-executable "xmllint")
  )
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq-default doxymacs-doxygen-style "C++")
(setq doxymacs-doxygen-style "JavaDoc")

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
(GNUEmacs
    (dolist (mode '(c-mode
                    java-mode
                    cperl-mode
                    html-mode-hook
                    css-mode-hook
                    emacs-lisp-mode))
      (font-lock-add-keywords mode
                              '(("\\(XXX\\|FIXME\\|TODO\\)"
                                 1 font-lock-warning-face prepend)))))

;;--------------------------------------------------------------------------------
;; polskie ustawienia
;;--------------------------------------------------------------------------------
;;(codepage-setup '1250)
;;(codepage-setup '852)
(if running-ms-windows
    (progn
      (set-keyboard-coding-system 'cp1250)
      (set-default-coding-systems 'cp1250)
      )
  )

;;--------------------------------------------------------------------------------
;; rozpoznawanie odpowiednich końcówek linii plików tekstowych
;;--------------------------------------------------------------------------------
(defun set-buffer-file-eol-type (eol-type)
  "Set the file end-of-line conversion type of the current buffer to
 EOL-TYPE.
 This means that when you save the buffer, line endings will be converted
 according to EOL-TYPE.

 EOL-TYPE is one of three symbols:

   unix (LF)
   dos (CRLF)
   mac (CR)

 This function marks the buffer modified so that the succeeding
 \\[save-buffer]
 surely saves the buffer with EOL-TYPE.  From a program, if you don't want
 to mark the buffer modified, use coding-system-change-eol-conversion
 directly [weikart]."
  (interactive "SEOL type for visited file (unix, dos, or mac): ")
  (setq buffer-file-coding-system (coding-system-change-eol-conversion
                                   buffer-file-coding-system eol-type))
  (set-buffer-modified-p t)
  (force-mode-line-update))

(global-set-key "\^Cu" (lambda () (interactive) (set-buffer-file-eol-type 'unix)))
(global-set-key "\^Cd" (lambda () (interactive) (set-buffer-file-eol-type 'dos)))
(global-set-key "\^Cm" (lambda () (interactive) (set-buffer-file-eol-type 'mac)))


;; Make the mode-line display the standard EOL-TYPE symbols (used above)...
(setq eol-mnemonic-undecided "(?)" ;; unknown EOL type
      eol-mnemonic-unix  "(unix)" ;; LF
      eol-mnemonic-dos  "(dos)" ;; CRLF
      eol-mnemonic-mac  "(mac)") ;; CR

;;--------------------------------------------------------------------------------
;; gnuserv
;;--------------------------------------------------------------------------------
(if running-ms-windows
    (progn
      (setq server-program "d:/karol/programy/emacs/bin/gnuserv.exe")
      (require 'gnuserv)

;;; open buffer in existing frame instead of creating new one...
      (setq gnuserv-frame (selected-frame))
      (message "gnuserv started.")
      (gnuserv-start)
      )
  (progn
    (server-start))
  )

;;--------------------------------------------------------------------------------
;; pokazuj krańcowe nawiasy
;;--------------------------------------------------------------------------------
(show-paren-mode 1)
(setq transient-mark-mode nil)

;;--------------------------------------------------------------------------------
;; % key on paren moves cursor to matching paren
;;--------------------------------------------------------------------------------
(global-set-key [(%)] 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;--------------------------------------------------------------------------------
;;
;;--------------------------------------------------------------------------------
(setq vc-follow-symlinks t
      visible-bell t)

;;--------------------------------------------------------------------------------
;; GUD mode
;;--------------------------------------------------------------------------------
(setq gdb-many-windows t)

;;----------------------------------------------------------------------
;; clearcase mode
;;----------------------------------------------------------------------
(require 'clearcase)

;;----------------------------------------------------------------------
;; Subversion mode
;;----------------------------------------------------------------------
(require 'psvn)
(setq svn-user-names-including-blanks '("Karol Barski"
                                        "Jerzy Myc"
                                        "Tomasz Waligora"
                                        "Zbigniew Zagorski")) ; username used on SAL9000 contains blanks
(add-hook 'svn-pre-parse-status-hook 'svn-status-parse-fixup-user-names-including-blanks)

;;----------------------------------------------------------------------
;; git mode
;;----------------------------------------------------------------------
(require 'git)
(require 'git-blame)

;;--------------------------------------------------------------------------------
;; styl indentacji kodu
;;--------------------------------------------------------------------------------
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "stroustrup")
        (other . "bsd")))

;(setq c-set-style "ellmtel")

;;--------------------------------------------------------------------------------
;; użycie tabulatora
;;--------------------------------------------------------------------------------
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;; (require 'cc-mode)
;; ;; personal preferences
;; (c-set-offset 'substatement-open 0)
;; (c-set-offset 'case-label '+)
;; (c-set-offset 'arglist-cont-nonempty '+)
;; (c-set-offset 'arglist-intro '+)
;; (c-set-offset 'topmost-intro-cont '+)

;;--------------------------------------------------------------------------------
;; automatyczny odczyt plików kompresowanych
;;--------------------------------------------------------------------------------
(auto-compression-mode 1)

;;--------------------------------------------------------------------------------
;; zegarek
;;--------------------------------------------------------------------------------
(display-time)
(setq display-time-format "%H:%M %d/%m/%Y")
(setq display-time-24hr-format t)

;;--------------------------------------------------------------------------------
;; turn off blinking cursor
;;--------------------------------------------------------------------------------
(blink-cursor-mode -1)

;;--------------------------------------------------------------------------------
;; klawisze skrótów
;;--------------------------------------------------------------------------------
(global-set-key [(meta g)] 'goto-line)
;; (global-set-key [(meta G)] 'what-line)

(if (string-equal "21" (substring emacs-version 0 2))
    (progn
      ;; ===== Function to delete a line =====

      ;; First define a variable which will store the previous column position
      (defvar previous-column nil "Save the column position")

      ;; Define the nuke-line function. The line is killed, then the newline
      ;; character is deleted. The column which the cursor was positioned at is then
      ;; restored. Because the kill-line function is used, the contents deleted can
      ;; be later restored by usibackward-delete-char-untabifyng the yank commands.
      (defun nuke-line()
        "Kill an entire line, including the trailing newline character"
        (interactive)

        ;; Store the current column position, so it can later be restored for a more
        ;; natural feel to the deletion
        (setq previous-column (current-column))

        ;; Now move to the end of the current line
        (end-of-line)

        ;; Test the length of the line. If it is 0, there is no need for a
        ;; kill-line. All that happens in this case is that the new-line character
        ;; is deleted.
        (if (= (current-column) 0)
            (delete-char 1)

          ;; This is the 'else' clause. The current line being deleted is not zero
          ;; in length. First remove the line by moving to its start and then
          ;; killing, followed by deletion of the newline character, and then
          ;; finally restoration of the column position.
          (progn
            (beginning-of-line)
            (kill-line)
            (delete-char 1)
            (move-to-column previous-column))))

      ;; kill whole line with C-; (because ; is close to k)
      (global-set-key [(ctrl \;)] 'nuke-line)
      )
  
  ;; kill whole line with C-; (because ; is close to k)
  (global-set-key [(ctrl \;)] 'kill-whole-line)
  
  )

;; string-insert-rectangle is useful but not binded to any key by default
(global-set-key (kbd "C-x r a") 'string-insert-rectangle)

;;--------------------------------------------------------------------------------
;; CUA mode
;;--------------------------------------------------------------------------------
(require 'cua)
(CUA-mode 'emacs)

;;--------------------------------------------------------------------------------
;; zezwalaj na użycie poniższych komend
;;--------------------------------------------------------------------------------
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; keep minibuffer history between session
(if (not (string-equal "21" (substring emacs-version 0 2)))
    (savehist-mode t)
  )


;; autocompletion
(global-set-key (kbd "ESC ESC") 'dabbrev-expand) ; ESC ESC ESC not usable :-/
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

;; ;; textmate-next-line from textmate.el - github.com/defunkt/textmate.el
;; (defun textmate-next-line ()
;;   "Go to next line and indent wherever you are in a line"
;;   (interactive)
;;   (end-of-line)
;;   (newline-and-indent))

;; (global-set-key [C-return] 'textmate-next-line)

;; ;; textmate-semicolon-and-next-line
;; (defun textmate-semicolon-and-next-line ()
;;   "Put a semicolon at the end of the line where you are then
;; go to next line and indent wherever you are in a line"
;;   (interactive)
;;   (end-of-line)
;;   (insert ";")
;;   (newline-and-indent))
;; (global-set-key [M-return] 'textmate-semicolon-and-next-line)

;; comment-or-uncomment-region-or-line
; it's almost the same as in textmate.el but I wrote it before I know about
; textmate.el, in fact that's how I found textmate.el, by googling this
; function to see if somebody already did that in a better way than me.
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
        (line-beginning-position) (line-end-position))
      (if (< (point) (mark))
          (comment-or-uncomment-region (point) (mark))
        (comment-or-uncomment-region (mark) (point)))))
(global-set-key (kbd "C-'") 'comment-or-uncomment-region-or-line)

;;--------------------------------------------------------------------------------
;; function that inserts incrementing numbers in place
;;--------------------------------------------------------------------------------
(defun insert-numbers (min max)
  "Insert numbers incrementing in lines at point."
  (interactive "nFrom: \nnTo: ")
  (let ((insertcolumn (current-column))
        (first t))
    (push-mark)
    (while (<= min max)
      (or first
          (progn
            (forward-line 1)
            (or (bolp) (insert ?\n))
            (move-to-column-force insertcolumn)))
      (setq first nil)
      (insert (format "%d" min))
      (setq min (+ 1 min)))))

;; (defun insert-numbers (min max)
;;   (interactive "nFrom: \nnTo: ")
;;   (let ((margin (buffer-substring (save-excursion (beginning-of-line) (point))
;;                                (point))))
;;  (when (<= min max)
;;    (insert (format "%d" min))
;;    (setq min (+ 1 min))
;;    (while (<= min max)
;;      (insert (format "\n%s%d" margin min))
;;      (setq min (+ 1 min))))))


;; ido!
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; scrolling settings
(setq scroll-preserve-screen-position t)
(setq scroll-margin 2)
(setq scroll-step 1)

;; files should always end with a new line
;; (setq require-final-newline t)

;; no automatic saving and ugly #file#
;; (setq auto-save-default nil)

;; tramp claque (really bad french play-on-word)
;(require 'tramp)
;(setq tramp-default-method "scp")

;; xml-mode is better than nxml-mode or html-mode
(add-to-list 'auto-mode-alist
  '("\\.html\\'\\|\\.xml\\'\\|\\.phtml\\'" . xml-mode))

;; outline-mode
(add-to-list 'auto-mode-alist
  '("\\.list\\'" . outline-mode))

;; don't let Customize mess with my .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;-------------------------------------------------------------------------------
;; python mode
;;-------------------------------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;;--------------------------------------------------------------------------------
;; ruby mode
;;--------------------------------------------------------------------------------
(if (or (string-equal (getenv "OSTYPE") "Linux")
        (string-equal (getenv "OSTYPE") "linux")
		(string-equal (getenv "OSTYPE") "linux-gnu"))
    (setq ruby-program-name "/usr/bin/ruby")
  (or (string-equal (getenv "OSTYPE") "FreeBSD" ))
  (setq ruby-program-name "/usr/local/bin/ruby"))
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

(require 'ruby-electric)
