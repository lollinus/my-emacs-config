;;; json-pretty-print-array.el --- Pretty print each element in a JSON array  -*- lexical-binding: t; -*-

;; Author: qxz2st8
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: json, tools
;; URL: https://github.com/qxz2st8/.emacs.d

;;; Commentary:

;; Provides two commands for reformatting a JSON array or object at point:
;;
;; `json-pretty-print-array-elements' - Pretty prints each element (array)
;; or member value (object) with full indentation.
;;
;; `json-pretty-print-array-elements-minimize' - Formats the collection with
;; each element/member value collapsed to a compact single-line string.
;;
;; Both commands detect whether point is inside an array or an object and
;; format accordingly, replacing only that region in the buffer.

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

(defun json-ppa--indent-value (value)
  "Return VALUE encoded and pretty-printed with 2-space base indent.
All lines are indented; the leading whitespace on the first line is stripped
so callers can place the first line inline after a key or bracket."
  (let* ((raw (with-temp-buffer
                (insert (json-encode value))
                (goto-char (point-min))
                (json-pretty-print-buffer)
                (goto-char (point-min))
                (while (not (eobp))
                  (insert "  ")
                  (forward-line 1))
                (string-trim-right
                 (buffer-substring-no-properties (point-min) (point-max)))))
         (lines (split-string raw "\n")))
    (cons (string-trim-left (car lines)) (cdr lines))))

(defun json-ppa--format-pretty-member (key value)
  "Format object member KEY: VALUE with VALUE fully pretty-printed."
  (let* ((key-str (format "  %s: " (json-encode (symbol-name key))))
         (val-parts (json-ppa--indent-value value)))
    (mapconcat #'identity
               (cons (concat key-str (car val-parts))
                     (cdr val-parts))
               "\n")))

(defun json-ppa--format-minimize-member (key value)
  "Format object member KEY: VALUE with VALUE minified to one line."
  (format "  %s: %s"
          (json-encode (symbol-name key))
          (json-encode value)))

;;; Commands

;;;###autoload
(defun json-pretty-print-array-elements ()
  "Pretty print each element of the JSON array or object at point.
For arrays, each element is formatted with full indentation.
For objects, each member value is fully pretty-printed."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds
      (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (collection (json-ppa--read-collection-at beg end))
           (arrayp (json-ppa--array-p beg))
           formatted-members)
      (if arrayp
          (dolist (element collection)
            (let ((val-parts (json-ppa--indent-value element)))
              (push (mapconcat #'identity
                               (cons (concat "  " (car val-parts))
                                     (cdr val-parts))
                               "\n")
                    formatted-members)))
        (dolist (pair collection)
          (push (json-ppa--format-pretty-member (car pair) (cdr pair))
                formatted-members)))
      (delete-region beg end)
      (goto-char beg)
      (insert (if arrayp "[\n" "{\n"))
      (insert (mapconcat #'identity (nreverse formatted-members) ",\n"))
      (insert (if arrayp "\n]" "\n}")))))

;;;###autoload
(defun json-pretty-print-array-elements-minimize ()
  "Format the JSON array or object at point with each element/value minified.
The outer brackets span multiple lines, but each element (array) or member
value (object) is collapsed to a compact single-line representation."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds
      (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (collection (json-ppa--read-collection-at beg end))
           (arrayp (json-ppa--array-p beg))
           formatted-members)
      (if arrayp
          (dolist (element collection)
            (push (concat "  " (json-encode element)) formatted-members))
        (dolist (pair collection)
          (push (json-ppa--format-minimize-member (car pair) (cdr pair))
                formatted-members)))
      (delete-region beg end)
      (goto-char beg)
      (insert (if arrayp "[\n" "{\n"))
      (insert (mapconcat #'identity (nreverse formatted-members) ",\n"))
      (insert (if arrayp "\n]" "\n}")))))

(provide 'json-pretty-print-array)
;;; json-pretty-print-array.el ends here
