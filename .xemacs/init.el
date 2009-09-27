;(setq user-mail-address "kbarski@ps.pl")
;(setq user-full-name "Karol Barski")

;; Sprawdza czy u¿ywamy XEmacsa czy FSF Emacsa
;; na podstawie "mike's very fine .emacs" (z www.dotfiles.com)
(defvar running-xemacs 
  (string-match "XEmacs\\|Lucid" emacs-version))

;;----------------------------------------------------------------------
;; dodatkowe modu³y do emacsa 
(setq load-path (nconc load-path (list "~/elisp"
									   "i:/programy/emacsen/site-lisp"
									   "/usr/local/share/emacs/site-lisp/gnuserv"
									   ))
;;									   "C:/programy/emacsen/site-lisp/auctex"))
      )

;;----------------------------------------------------------------------
;; gnus 
;(require 't-gnus-startup)

;;----------------------------------------------------------------------
;; pf.conf
;;(load "pf-mode")
;;(require 'pf-mode)


;; moje zmienne
;;----------------------------------------------------------------------
;; co to za system
(message "loading what-env...")
(load "what-env")
(message "loading what-env... done.")
;; ju¿ wiem

;;----------------------------------------------------------------------
;;(message "jsp mode from www.xanadb.com/archive/emacs/20040622/")
;;(load "jsp_mode")
;; jsp wyko¿ystuj±cy multi-mode.el
(load "multi-mode")
(defun jsp-mode () (interactive)
  (multi-mode 1
	      'html-mode
	      '("<%--" indented-text-mode)
	      '("<%@" indented-text-mode)
	      '("<%=" html-mode)
	      '("<%" java-mode)
	      '("%>" html-mode)))

(add-to-list 'auto-mode-alist '("\\.jsp.?$" . jsp-mode))
(add-to-list 'auto-mode-alist '("\\.tag.?$" . jsp-mode))

;;----------------------------------------------------------------------
;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
;; Detect EOL mode by looking for CR/LF on the first line
;; (add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
;; (add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

;; Add missing support functions
;; (defun utf-16-le-pre-write-conversion (start end) nil)
;; (defun utf-16-be-pre-write-conversion (start end) nil)

;;----------------------------------------------------------------------
;;ustawienia regionalne

;;(set-default-coding-systems           'iso-latin-2)
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

;;(set-input-method 'latin-2-postfix)
;; (codepage-setup '1250)
;; (codepage-setup '852)
;; (define-coding-system-alias 'windows-1250 'cp1250)
(setq tool-bar-mode t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
(setq frame-title-format "Emacs: %b %+%+ %f ")

;;----------------------------------------------------------------------
;; Put the following forms in your .emacs to enable autoloading of JavaScript
;; mode, and auto-recognition of ".js" files.

(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
(setq auto-mode-alist (append '(("\\.js$" . javascript-mode))
                                auto-mode-alist))

;;----------------------------------------------------------------------
;; css-editing mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;-------------------------------------------------------------------------------
;; python mode 
;;(setq auto-mode-alist
;;      (cons '("\\.py$" . python-mode) auto-mode-alist))
;;(setq interpreter-mode-alist
;;      (cons '("python" . python-mode)
;;            interpreter-mode-alist))
;;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;--------------------------------------------------------------------
;; ruby mode
;; je¿eli linux to w pracy /usr/bin/ruby
;; je¿eli Freebsd to w domu /usr/local/bin/ruby
(if (or (string-equal (getenv "OSTYPE") "Linux")
        (string-equal (getenv "OSTYPE") "linux")
		(string-equal (getenv "OSTYPE") "linux-gnu"))
    (setq ruby-program-name "/usr/bin/ruby")
  (or (string-equal (getenv "OSTYPE") "FreeBSD" ))
  (setq ruby-program-name "/usr/local/bin/ruby"))
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;;----------------------------------------------------------------------
;; wget mode
;;(autoload 'wget "wget" "wget interface for Emacs." t)
;;(autoload 'wget-web-page "wget" "download web page." t)

;;----------------------------------------------------------------------
;; php mode 
(autoload 'php-mode "php-mode" "Mode for editing PHP source files")
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))

;;----------------------------------------------------------------------
;; html helper mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)
(setq tempo-interactive t)
(add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
(add-hook 'html-helper-mode-hook '(lambda () (font-lock-mode 1)))
(setq html-helper-address-string 
      "<a href="mailto://kbarski<AT>ps.pl/">Karol Barski &lt;kbarski&lt;at&gt;ps.pl;</a>")

(defun html-helper-lolo-insert-timestamp () 
  "Default timestamp insertion function" 
  (insert "Ostatnio Zmienione: " 
          (current-time-string) 
          "\n"))
(setq html-helper-timestamp-hook 'html-helper-lolo-insert-timestamp)

;;-------------------------------------------------------------------------------
;;Ustawienia s³ownika u¿ywamy aspella zamiast standardowego ispella
;; (setq ispell-program-name "aspell")
;; (ispell-change-dictionary "polish" "english")
;; (setq ispell-enable-tex-parser t)
;;s³ownik skonfigurowany

;; Word Count tool, per sshteingold@cctrading.com
(if (not (string-equal system-name "server.localdomain" ))
    (progn
      (defun sds-word-count (start end)
	;; replacement for count-lines-region
	"Count lines/words/characters from START to END"
	(interactive "r")
	(save-excursion
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char (min start end))
	    (message "Region (%d to %d) has: lines: %d; words: %d; characters: %d."
		     start end (count-lines start end)
		     (string-to-number (how-many "\\<")) (- end start)))))
      (define-key esc-map "=" 'sds-word-count)
      ))

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

;;--------------------------------------------------------------------------------
;; Tab length
;;--------------------------------------------------------------------------------
(setq default-tab-width 4)
(setq-default indent-tabs-mode t)
;;(setq-default tab-width 4)
;;(setq-default indent-tabs-mode 1)
;; (require 'cc-mode)
;; ;; personal preferences
;; (c-set-offset 'substatement-open 0)
;; (c-set-offset 'case-label '+)
;; (c-set-offset 'arglist-cont-nonempty '+)
;; (c-set-offset 'arglist-intro '+)
;; (c-set-offset 'topmost-intro-cont '+)

;; location of netscape, for browse-url:
(setq browse-url-netscape-program "firefox")

;;this is just here in case i actually want to set it... which i don't
;;feel like doing right now.
(setq c-default-style "stroustrup")
(setq c-set-style "k&r")
;; I prefer BSD style C indentation over the default GNU style. Other choices
;; are gnu, k&r, whitesmith, etc. You can change this on fly using
;;M-x c-set-style. Use tab to see the options. 

;; Auto fill in all major modes
;;(setq-default auto-fill-function 'do-auto-fill)

;; Automagically read compressed files
(auto-compression-mode 1)

;;Scroll just one line when hitting the bottom of the window
;;no more jumping a page - irritating
(setq scroll-step 1)

;; Show line and column numbers in modeline
(line-number-mode t)
(column-number-mode t)

;; Turn on the matching parentheses feature
;;
;; (global-set-key "\C-x p" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert `."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
 		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
 		(t (self-insert-command (or arg 1)))))


;; misc
;;
(display-time)
(setq display-time-24hr-format t)

;;;--------------------------------------------------------------------------------
;; klawisze skrótów
;;--------------------------------------------------------------------------------
;; (global-set-key "\M-g" 'goto-line)

;;(setq next-line-add-newlines nil)
;;(setq track-eol t)
;;(setq require-final-newline t)
;;(setq kill-whole-line t)
;;(load-library "paren")
;;(setq line-number-mode t)

;; Highlight marked region
;; (transient-mark-mode 0)
;;(show-paren-mode t)

;;gnuserv ----------------------------------------
;; start gnuserv stuff. Put it up front so you don't get into a
;; timeout problem when starting emacs from gnuclient or gnudoit,
;; according to Cristian Ionescu-Idbohrn
;; <cristian.ionescu-idbohrn@axis.com>
;; (setenv "GNUSERV_SHOW_EMACS" "1")
;; (autoload 'gnuserv-start "gnuserv-compat"
;;   "Allow this Emacs process to be a server for client processes." t)
;; (if (or (string-equal (getenv "OSTYPE") "Linux") 
;;         (string-equal (getenv "OSTYPE") "linux")
;; 		(string-equal (getenv "OSTYPE") "linux-gnu"))
;;     (setq gnuserv-program "gnuserv")
;;   (string-equal (getenv "OSTYPE") "FreeBSD" )
;;   (setq gnuserv-program "gnuserv-emacs")
;;   )

;;(gnuserv-start)

;; (if (memq window-system '(win32 w32))	; Windows NT/95
;;     (progn
;;       (setq server-program "i:/programy/emacs/bin/gnuserv.exe" )
;;       (require 'gnuserv)
;;       (setq server-done-function 'bury-buffer
;; 			gnuserv-frame (car (frame-list)))
;;       (gnuserv-start)
;;       ;;; open buffer in existing frame instead of creating new one...
;;       (setq gnuserv-frame (selected-frame))
;;       (message "gnuserv started.")))

;; (if (or (string-equal (getenv "OSTYPE") "Linux") 
;;         (string-equal (getenv "OSTYPE") "linux")
;; 		(string-equal (getenv "OSTYPE") "linux-gnu")
;; 		(string-equal (getenv "OSTYPE") "FreeBSD" ))
;;     (gnuserv-start))

;; (defadvice server-visit-files (around save-buffers last activate)
;;   "Try to emulate gnuclient behaviour with emacsclient.
;;         Works only for visiting one buffer at a time."
;;   (let* ((filen (car (car (ad-get-arg 0))))
;; 		 (buf (get-file-buffer filen))
;; 		 (this-buf-modified-p nil))
;;             ;;; the following is copied from server-visit-files, with
;;             ;;; a modification for the `verify-visited-file-modtime' test
;; 	(if (and buf (set-buffer buf))
;; 		(if (file-exists-p filen)
;;                     ;;; if the file has changed on disk, reload it
;;                     ;;; using `find-file-noselect'
;; 			(if (not (verify-visited-file-modtime buf))
;; 				(progn
;; 				  (find-file-noselect filen)
;;                           ;;; if user answered `no', reset modtime anyway
;;                           ;;; so that server-visit-files doesn't realize the
;;                           ;;; difference:
;; 				  (set-visited-file-modtime)))
;;                   ;;; if file exists no longer, we let server-visit-files
;;                   ;;; deal with that
;; 		  t)
;; 	  (setq buf (find-file-noselect filen)))
;; 	(setq this-buf-modified-p (buffer-modified-p buf))
;; 	(set-buffer buf)
;; 	(set-buffer-modified-p nil)
;; 	ad-do-it
;; 	(set-buffer-modified-p this-buf-modified-p)))

;;  (server-start))


;; end gnuserv stuff

;; auc tex ----------------------------------------------------------------
;; (require 'tex-site)
;;-------------------------------------------------------------------------
;; hilit higlight mode for auctex -----------------------------------------
;;(if window-system
;;        (require 'hilit-LaTeX))

;; ;;-------------------------------------------------------------------------
;; ;; Kolorowanie sk³adni
(require 'color-theme)
(if window-system 
	(color-theme-subtle-hacker)
  (color-theme-arjen))
;; Blinking parenthesis
;; (mo¿na wy³±czyæ je¶li komu przeszkadza).
(if running-xemacs
    (progn                            ;XEmacs
      (paren-set-mode 'paren)           ;pod¶wietlanie nawiasów
      )
  (progn                              ;FSF Emacs:
    (require 'paren)                    ;³adujemy bibliotekê
    (show-paren-mode t)                 ;w³±czamy pod¶wietlanie paruj±cych nawiasów
    )
  )

;;--------------------------------------------------------------------------------
;; iswitchb-mode for interactive switch buffers
;;--------------------------------------------------------------------------------
(iswitchb-mode 1)

;;--------------------------------------------------------------------------------
;; % key on paren moves cursor to matching paren
;;--------------------------------------------------------------------------------
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;;--------------------------------------------------------------------------------
;; svn mode
;;--------------------------------------------------------------------------------
;; (require 'psvn)
;; (add-to-list 'vc-handled-backends 'SVN)

;;--------------------------------------------------------------------------------
;; 
;;--------------------------------------------------------------------------------
(setq vc-follow-symlinks t
      visible-bell t)

;; Font-lock-mode settings (kolorowanie sk³adni)
(if running-xemacs
    (progn                                      ;XEmacs
      (turn-on-font-lock)                         ;w³±czenie kolorków globalnie
      (lazy-shot-mode)                            ;'leniwe' kolorowanie
      )
  (progn                                        ;FSF Emacs
    (global-font-lock-mode t)                     ;w³±czenie kolorków globalnie
;;    (setq font-lock-support-mode 'lazy-lock-mode) ;'leniwe' kolorowanie
    )
)
(setq font-lock-maximum-decoration t)             ;kolorowanie na maksimum

;;--------------------------------------------------------------------------------
;; modu³ do edycji i debgowania Caml'a
;; tuareg mode 
;; (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; (if (and (boundp 'window-system) window-system)
;;     (when (string-match "XEmacs" emacs-version)
;; 	  (if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
;; 		  (require 'sym-lock))
;; 	  (require 'font-lock)))
;; --------------------------------------------------------------------------------

;; xml-mode ----------------------------------------------------------
;; (setq load-path (cons (expand-file-name "c:/programy/emacs/sxml-mode") load-path))
;; (autoload 'sxml-mode "sxml-mode" "Major mode for editing XML documents." t)
;; Invokes automatically the XML mode with the .xml extension
;; (setq auto-mode-alist
;;      (cons '("\\.xml$" . sxml-mode) auto-mode-alist)) 
;;--------------------------------------------------------------------

;;(defvar font-lock-mode-list 
;;  (list 'cc-mode 'c-mode 'c++-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode 'latex-mode 'tex-mode)
;;	"List of modes to always start in font-lock-mode")

;;(add-hook 'tex-mode-hook	'turn-on-font-lock)
;;(add-hook 'emacs-lisp-mode-hook	'turn-on-font-lock)
;;(add-hook 'dired-mode-hook 'turn-on-font-lock)
;;(add-hook 'cc-mode-hook	'turn-on-font-lock)
;;(add-hook 'c-mode-hook 'turn-on-font-lock)
;;(add-hook 'lisp-mode-hook 'turn-on-font-lock)
;;(add-hook 'perl-mode-hook 'turn-on-font-lock)
;;(add-hook 'scheme-mode-hook 'turn-on-font-lock)
;;(add-hook '-mode-hook 'turn-on-font-lock)
;;(add-hook 'c++-mode-hook 'turn-on-font-lock)

;;----------------------------------------------------------------------
;; zezwalaj na u¿ywanie C-x n n 
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------
;; Haskell mode
;;---------------------------------------------
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)


;;--------------------------------------------------------------------------------
;; automatyczny odczyt plików kompresowanych
;;--------------------------------------------------------------------------------
(auto-compression-mode 1)

;;--------------------------------------------------------------------------------
;; doxygen mode
;;--------------------------------------------------------------------------------
;;(require 'doxygen)
;;(setq doxymacs-doxygen-dirs "c:/Karol/Programy/doxygen/bin")
(require 'doxymacs)
(setq doxymacs-use-external-xml-parser t)
(if (memq window-system '(win32 w32))	; Windows NT/95
	(setq doxymacs-external-xml-parser-executable "xmllint.exe")
   (setq doxymacs-external-xml-parser-executable "xmllint")
  )

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;;(setq-default doxymacs-doxygen-style "C++")
(setq doxymacs-doxygen-style "JavaDoc")
