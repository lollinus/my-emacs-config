;;; rc-functions.el --- Useful functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Various useful function used during Emacs startup and runtime

;;; Code:
;; using cursor color to indicate some modes (read-only, insert and
;; overwrite modes)
(defvar kb/set-cursor-color-color "")
(defvar kb/set-cursor-color-buffer "")

(defun kb/set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
	 (if buffer-read-only "purple1"
	   (if overwrite-mode "red"
	     "rgb:15/FF/00"))))  ;; insert mode
    (unless (and (string= color kb/set-cursor-color-color)
		 (string= (buffer-name) kb/set-cursor-color-buffer))
      (set-cursor-color (setq kb/set-cursor-color-color color))
      (setq kb/set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'kb/set-cursor-color-according-to-mode)

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "Kill region or line.
;; When called interactively with no active region, copy a single
;; line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single
;;   line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

(defun kb/check-position ()
  "Kill whole line.

Include whitespace characters until next non-whiepsace character of next line."
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; (advice-add 'kill-line :before 'kb/check-position)
;; (advice-remove 'kill-line 'kb/check-position)

;; ;; taken from prelude-editor.el
;; ;; automatically indenting yanked text if in programming-modes
;; (defvar yank-indent-modes
;;   '(LaTeX-mode TeX-mode)
;;   "Modes in which to indent regions that are yanked (or yank-popped).
;; Only modes that don't derive from `prog-mode' should be listed here.")

;; (defvar yank-indent-blacklisted-modes
;;   '(python-mode slim-mode haml-mode)
;;   "Modes for which auto-indenting is suppressed.")

;; (defvar yank-advised-indent-threshold 1000
;;   "Threshold (# chars) over which indentation does not automatically occur.")

;; (defun yank-advised-indent-function (beg end)
;;   "Do indentation, as long as the region isn't too large.

;; BEG beginning of a insert region.
;; END of a inert region."
;;   (if (<= (- end beg) yank-advised-indent-threshold)
;;       (indent-region beg end nil)))

;; (defadvice yank (after yank-indent activate)
;;   "If current mode is one of 'yank-indent-modes.
;; Indent yanked text (with prefix arg don't indent)."
;;   (if (and (not (ad-get-arg 0))
;;            (not (member major-mode yank-indent-blacklisted-modes))
;;            (or (derived-mode-p 'prog-mode)
;;                (member major-mode yank-indent-modes)))
;;       (let ((transient-mark-mode nil))
;;         (yank-advised-indent-function (region-beginning) (region-end)))))

;; (defadvice yank-pop (after yank-pop-indent activate)
;;   "If current mode is one of `yank-indent-modes', indent yanked text.

;;  With prefix arg don't indent."
;;   (when (and (not (ad-get-arg 0))
;;              (not (member major-mode yank-indent-blacklisted-modes))
;;              (or (derived-mode-p 'prog-mode)
;;                  (member major-mode yank-indent-modes)))
;;     (let ((transient-mark-mode nil))
;;       (yank-advised-indent-function (region-beginning) (region-end)))))

;; ;; prelude-core.el
;; (defun indent-buffer ()
;;   "Indent the currently visited buffer."
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; ;; prelude-editing.el
;; (defcustom prelude-indent-sensitive-modes
;;   '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
;;   "Modes for which auto-indenting is suppressed."
;;   :type 'list)

;; (defun indent-region-or-buffer ()
;;   "Indent a region if selected, otherwise the whole buffer."
;;   (interactive)
;;   (unless (member major-mode prelude-indent-sensitive-modes)
;;     (save-excursion
;;       (if (region-active-p)
;;           (progn
;;             (indent-region (region-beginning) (region-end))
;;             (message "Indented selected region."))
;;         (progn
;;           (indent-buffer)
;;           (message "Indented buffer.")))
;;       (whitespace-cleanup))))

;; ;; add duplicate line function from Prelude
;; ;; taken from prelude-core.el
;; (defun prelude-get-positions-of-line-or-region ()
;;   "Return positions (beg . end) of the current line
;; or region."
;;   (let (beg end)
;;     (if (and mark-active (> (point) (mark)))
;;         (exchange-point-and-mark))
;;     (setq beg (line-beginning-position))
;;     (if mark-active
;;         (exchange-point-and-mark))
;;     (setq end (line-end-position))
;;     (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;;;
;;; Matching parentheses
;;;
(defun match-parenthesis (arg)
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

	(global-set-key \"\\C-c6\" #\='match-parenthesis)
	(global-set-key [f6] #\='match-parenthesis)

Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  (let
      ((syntax (char-syntax (following-char))))
    (cond
     ((= syntax ?\()
      (forward-sexp 1) (backward-char))
     ((= syntax ?\))
      (forward-char) (backward-sexp 1))
     (t (self-insert-command (or arg 1)))
     )
    ))

(defun kb/delete-trailing-whitespaces-and-untabify ()
  "Delete trailing whitespace.

Delete all the trailing white spaces, and convert all tabs to
multiple spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  )

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
;; (if (not (fboundp 'utf-16-le-pre-write-conversion))
;;     (defun utf-16-le-pre-write-conversion (beg end)
;;       "Semi-dummy pre-write function effectively to autoload ucs-tables.
;; BEG begining of conversion region.
;; END end of conversion region."
;;       ;; Ensure translation table is loaded, if available.
;;       (require 'ucs-tables nil t)
;;       ;; Don't do this again.
;;       (coding-system-put 'utf-16-le 'pre-write-conversion nil)
;;       nil))

;; (if (not (fboundp 'utf-16-be-pre-write-conversion))
;;     (defun utf-16-be-pre-write-conversion (start end)
;;       "Semi-dummy pre-write function effectively to autoload ucs-tables.
;; BEG begining of conversion region.
;; END end of conversion region."
;;       ;; Ensure translation table is loaded, if available.
;;       (require 'ucs-tables nil t)
;;       ;; Don't do this again.
;;       (coding-system-put 'utf-16-be 'pre-write-conversion nil)
;;       nil))

;;--------------------------------------------------------------------------------
;; rozpoznawanie odpowiednich końcówek linii plików tekstowych
;;--------------------------------------------------------------------------------
(defun set-buffer-file-eol-type (eol-type)
  "Set the file `end-of-line' conversion type of the current buffer to EOL-TYPE.
This means that when you save the buffer,
line endings will be converted according to EOL-TYPE.

 EOL-TYPE is one of three symbols:

   unix (LF)
   dos (CRLF)
   mac (CR)

 This function marks the buffer modified so that the succeeding
 \\[save-buffer]
 surely saves the buffer with EOL-TYPE.  From a program, if you don't want
 to mark the buffer modified, use `coding-system-change-eol-conversion'
 directly [weikart]."
  (interactive (list (completing-read "SEOL type for visited file (unix, dos, or mac): " '("unix" "dos" "mac"))))
  (setq buffer-file-coding-system (coding-system-change-eol-conversion
                                   buffer-file-coding-system eol-type))
  (set-buffer-modified-p t)
  (force-mode-line-update))

;; Make the mode-line display the standard EOL-TYPE symbols (used above)...
;; (setq eol-mnemonic-undecided "(?)" ;; unknown EOL type
;;       eol-mnemonic-unix  "(unix)" ;; LF
;;       eol-mnemonic-dos  "(dos)" ;; CRLF
;;       eol-mnemonic-mac  "(mac)") ;; CR

(when (not (fboundp 'kill-whole-line))
  (progn
    ;; ===== Function to delete a line =====

    ;; First define a variable which will store the previous column position
    (defvar previous-column nil "Save the column position")

    ;; Define the kb/kill-whole-line function. The line is killed, then the newline
    ;; character is deleted. The column which the cursor was positioned at is then
    ;; restored. Because the kill-line function is used, the contents deleted can
    ;; be later restored by usibackward-delete-char-untabifyng the yank commands.
    (defun kb/kill-whole-line()
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
    )
  )

(defun kb/insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (format-time-string "%Y-%m-%d"))
  )

(defun kb/insert-date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2012-06-10T21:11:33+02:00
See: URL `http://en.wikipedia.org/wiki/ISO_8601'"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

(provide 'rc-functions)

;;; rc-functions.el ends here
