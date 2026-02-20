;;; json-pretty-print-array.el --- Pretty print members of a JSON collection  -*- lexical-binding: t; -*-

;; Author: qxz2st8
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: json, tools
;; URL: https://github.com/qxz2st8/.emacs.d

;;; Commentary:

;; Provides two commands for reformatting a JSON array or object at point:
;;
;; `json-pretty-print-members' - Pretty prints each element (array) or member
;; value (object) with full indentation.
;;
;; `json-minimize-members' - Formats the collection with each element/member
;; value collapsed to a compact single-line string.
;;
;; Both commands detect whether point is inside an array or an object,
;; replace only that region, and derive indentation from the column of the
;; opening bracket so that nested collections are formatted correctly.

;;; Code:

(require 'json)

;;; Internal helpers

(defun json-ppa--collection-bounds ()
  "Return (BEG . END) of the innermost JSON array or object around point, or nil."
  (save-excursion
    (condition-case nil
        (progn
          (unless (looking-at "[\\[{]")
            (up-list -1 t t)
            (while (not (looking-at "[\\[{]"))
              (up-list -1 t t)))
          (let ((start (point)))
            (forward-sexp)
            (cons start (point))))
      (error nil))))

(defun json-ppa--read-collection-at (beg end)
  "Parse the JSON array or object between BEG and END.
Returns a list for arrays and an alist for objects."
  (let ((json-array-type 'list)
        (json-object-type 'alist))
    (json-read-from-string (buffer-substring-no-properties beg end))))

(defun json-ppa--array-p (beg)
  "Return non-nil if the JSON collection starting at BEG is an array."
  (save-excursion
    (goto-char beg)
    (looking-at "\\[")))

(defun json-ppa--indent-value (value member-indent)
  "Return VALUE encoded and pretty-printed, using MEMBER-INDENT as base.
Returns a cons (FIRST-LINE . REST-LINES).  Leading whitespace is stripped
from FIRST-LINE so callers can place it inline after a key or bracket."
  (let* ((raw (with-temp-buffer
                (insert (json-encode value))
                (goto-char (point-min))
                (json-pretty-print-buffer)
                (goto-char (point-min))
                (while (not (eobp))
                  (insert member-indent)
                  (forward-line 1))
                (string-trim-right
                 (buffer-substring-no-properties (point-min) (point-max)))))
         (lines (split-string raw "\n")))
    (cons (string-trim-left (car lines)) (cdr lines))))

(defun json-ppa--format-pretty-member (key value member-indent)
  "Format object member KEY: VALUE with VALUE fully pretty-printed.
MEMBER-INDENT is prepended to each line of the member."
  (let* ((key-str (format "%s%s: " member-indent (json-encode (symbol-name key))))
         (val-parts (json-ppa--indent-value value member-indent)))
    (mapconcat #'identity
               (cons (concat key-str (car val-parts))
                     (cdr val-parts))
               "\n")))

(defun json-ppa--format-minimize-member (key value member-indent)
  "Format object member KEY: VALUE with VALUE minified to one line.
MEMBER-INDENT is prepended to the member line."
  (format "%s%s: %s"
          member-indent
          (json-encode (symbol-name key))
          (json-encode value)))

;;; Commands

;;;###autoload
(defun json-pretty-print-members ()
  "Pretty print each element of the JSON array or object at point.
For arrays, each element is formatted with full indentation.
For objects, each member value is fully pretty-printed.
Indentation is derived from the column of the opening bracket."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds
      (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (base-col (save-excursion (goto-char beg) (current-column)))
           (member-indent (make-string (+ base-col 2) ?\s))
           (close-indent  (make-string base-col ?\s))
           (collection (json-ppa--read-collection-at beg end))
           (arrayp (json-ppa--array-p beg))
           formatted-members)
      (if arrayp
          (dolist (element collection)
            (let ((val-parts (json-ppa--indent-value element member-indent)))
              (push (mapconcat #'identity
                               (cons (concat member-indent (car val-parts))
                                     (cdr val-parts))
                               "\n")
                    formatted-members)))
        (dolist (pair collection)
          (push (json-ppa--format-pretty-member (car pair) (cdr pair) member-indent)
                formatted-members)))
      (delete-region beg end)
      (goto-char beg)
      (insert (if arrayp "[\n" "{\n"))
      (insert (mapconcat #'identity (nreverse formatted-members) ",\n"))
      (insert (concat "\n" close-indent (if arrayp "]" "}"))))))

;;;###autoload
(defun json-minimize-members ()
  "Format the JSON array or object at point with each element/value minified.
The outer brackets span multiple lines, but each element (array) or member
value (object) is collapsed to a compact single-line representation.
Indentation is derived from the column of the opening bracket."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds
      (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (base-col (save-excursion (goto-char beg) (current-column)))
           (member-indent (make-string (+ base-col 2) ?\s))
           (close-indent  (make-string base-col ?\s))
           (collection (json-ppa--read-collection-at beg end))
           (arrayp (json-ppa--array-p beg))
           formatted-members)
      (if arrayp
          (dolist (element collection)
            (push (concat member-indent (json-encode element)) formatted-members))
        (dolist (pair collection)
          (push (json-ppa--format-minimize-member (car pair) (cdr pair) member-indent)
                formatted-members)))
      (delete-region beg end)
      (goto-char beg)
      (insert (if arrayp "[\n" "{\n"))
      (insert (mapconcat #'identity (nreverse formatted-members) ",\n"))
      (insert (concat "\n" close-indent (if arrayp "]" "}"))))))

(provide 'json-pretty-print-array)
;;; json-pretty-print-array.el ends here
