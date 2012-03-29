;; -*- mode: lisp; coding: utf-8-unix -*-
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
;; transparent emacs window on M$
;;--------------------------------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(92 50))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(92 50))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

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

(global-set-key (kbd "C-c u") (lambda () (interactive) (set-buffer-file-eol-type 'unix)))
(global-set-key (kbd "C-c d") (lambda () (interactive) (set-buffer-file-eol-type 'dos)))
(global-set-key (kbd "C-c m") (lambda () (interactive) (set-buffer-file-eol-type 'mac)))


;; Make the mode-line display the standard EOL-TYPE symbols (used above)...
(setq eol-mnemonic-undecided "(?)" ;; unknown EOL type
      eol-mnemonic-unix  "(unix)" ;; LF
      eol-mnemonic-dos  "(dos)" ;; CRLF
      eol-mnemonic-mac  "(mac)") ;; CR


;;--------------------------------------------------------------------------------
;; gnuserv
;;--------------------------------------------------------------------------------
(if running-ms-windows
    (server-start)
  )

;;--------------------------------------------------------------------------------
;; chrome edit
;;--------------------------------------------------------------------------------
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;;--------------------------------------------------------------------------------
;; pokazuj krańcowe nawiasy
;;--------------------------------------------------------------------------------
(show-paren-mode 1)
(setq transient-mark-mode nil)

;;--------------------------------------------------------------------------------
;; % key on paren moves cursor to matching paren
;;--------------------------------------------------------------------------------
(global-set-key (kbd "%") 'match-paren)

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
; turn off clearcase for emacs 24 because of config error
;; (if (not running-ms-windows)
;;     (require 'clearcase)
;;   )

;;----------------------------------------------------------------------
;; Subversion mode
;;----------------------------------------------------------------------
(require 'psvn)
(setq svn-user-names-including-blanks '("Karol Barski"
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
(global-set-key (kbd "M-g") 'goto-line)
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
      (global-set-key (kbd "C-;") 'nuke-line)
      )

  ;; kill whole line with C-; (because ; is close to k)
  (global-set-key (kbd "C-;") 'kill-whole-line)

  )

;; string-insert-rectangle is useful but not binded to any key by default
(global-set-key (kbd "C-x r a") 'string-insert-rectangle)

;;--------------------------------------------------------------------------------
;; CUA mode
;;--------------------------------------------------------------------------------
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only t) ;; no transient mark mode
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(if (string-equal "21" (substring emacs-version 0 2))
   (progn
     (require 'cua)
     (CUA-mode 'emacs)
     )
  (cua-mode 'emacs)
  )

;; Join lines as in Vim
(defun my-join-line()
  "Join current and next line, remove tralinig spaces leaving only one. Similar to Vim Ctrl-j"
  (interactive)
  (join-line 'forward-line)
)
(global-set-key (kbd "C-j") 'my-join-line)

;;--------------------------------------------------------------------------------
;; zezwalaj na użycie poniższych komend
;;--------------------------------------------------------------------------------
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; keep minibuffer history between session
(if (not (string-equal "21" (substring emacs-version 0 2)))
    (savehist-mode t)
  )

;; autocompletion
(global-set-key (kbd "ESC ESC") 'dabbrev-expand) ; ESC ESC ESC not usable :-/
;(setq skeleton-pair t)
;(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

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

;;--------------------------------------------------------------------------------
;; recentf mode
;;--------------------------------------------------------------------------------
(recentf-mode t)
(global-set-key (kbd "<f8>") 'recentf-open-files)

;;--------------------------------------------------------------------------------
;; ido!
;;--------------------------------------------------------------------------------
;;(require 'ido)
(ido-mode t)
;; (setq ido-enable-flex-matching t)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(defun ido-recentf-open ()
  "Use 'ido-completing-read to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file:" recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; scrolling settings
;;(setq scroll-preserve-screen-position t)
;;(setq scroll-margin 2)
;;(setq scroll-step 1)

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

;;--------------------------------------------------------------------------------
;; outline-mode
;;--------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist
  '("\\.list\\'" . outline-mode))

;;--------------------------------------------------------------------------------
;; org-mode
;;--------------------------------------------------------------------------------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-ditaa-jar-path "~/java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")
(setq org-src-fontify-natively t)

;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(org-babel-do-load-languages 'org-babel-load-languages '(
                                                         (emacs-lisp . t)
                                                         (awk . t)
                                                         (sh . t)
                                                         (C . t)
                                                         (sql . t)
                                                         (latex . t)
                                                         (org . t)
                                                         )
                             )

;; (org-babel-do-load-languages
;;  (quote org-babel-load-languages)
;;  (quote ((emacs-lisp . t)
;;          (dot . t)
;;          (ditaa . t)
;;          (R . t)
;;          (python . t)
;;          (ruby . t)
;;          (gnuplot . t)
;;          (clojure . t)
;;          (sh . t)
;;          (ledger . t)
;;          (org . t)
;;          (plantuml . t)
;;          (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
;; (setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
;; (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; don't let Customize mess with my .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;--------------------------------------------------------------------------------
;; JavaScript mode
;;--------------------------------------------------------------------------------
(autoload 'js-mode "JavaScript" "Start js-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

