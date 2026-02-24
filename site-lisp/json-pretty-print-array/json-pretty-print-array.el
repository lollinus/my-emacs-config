;;; json-pretty-print-array.el --- Pretty print members of a JSON collection  -*- lexical-binding: t; -*-

;; Author: qxz2st8
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: json, tools
;; URL: https://github.com/qxz2st8/.emacs.d

;;; Commentary:

;; Provides three commands for reformatting a JSON array or object at point:
;;
;; `json-pretty-print-members'  - Fully expand every nested collection.
;; `json-compact-members'      - Keep one element/member per line but minify
;;                                each element's content to a single line.
;; `json-format-to-depth'       - Expand collections to a given depth; anything
;;                                deeper is minified to a single line.
;; `json-cleanup-at-point'      - Fix common issues in JSON-alike documents:
;;                                extra whitespace around colons, key and value
;;                                split across lines, double-outer-quoted strings
;;                                like ""TEXT"", then reformat the result.
;;
;; All commands derive indentation from the column of the opening bracket, so
;; nested collections are formatted correctly regardless of their position in
;; the buffer.

;;; Code:

(require 'json)

;;; Internal helpers — bounds & parsing

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
Uses a sentinel for null so that nil unambiguously means empty collection."
  (let ((json-array-type  'list)
        (json-object-type 'alist)
        (json-null        :json-null))
    (json-read-from-string (buffer-substring-no-properties beg end))))

;;; Internal helpers — recursive depth-aware formatting

(defun json-ppa--object-p (value)
  "Return non-nil if parsed VALUE is a JSON object (alist with symbol keys).
Distinguishes objects from arrays whose elements happen to be alists."
  (and (consp value)
       (consp (car value))
       (symbolp (caar value))))

(defun json-ppa--encode (value)
  "Minify VALUE to a single-line JSON string."
  (let ((json-null :json-null))
    (json-encode value)))

(defun json-ppa--format-value-at-depth (value depth base-col)
  "Format VALUE, expanding collections up to DEPTH levels deep.
BASE-COL is the column of the enclosing opening bracket.
Returns a string (possibly multi-line).

- Scalars are always minified.
- When DEPTH <= 0, a collection is also minified.
- When DEPTH > 0, a collection is expanded one level and recursed at DEPTH-1."
  (cond
   ((not (listp value))  (json-ppa--encode value))
   ((<= depth 0)         (json-ppa--encode value))
   ((null value) "[]")
   (t
    (let* ((is-object     (json-ppa--object-p value))
           (close-indent  (make-string base-col ?\s))
           formatted-members)
      (if is-object
          (dolist (pair value)
            (push (json-ppa--format-member-at-depth
                   (car pair) (cdr pair) (1- depth) base-col)
                  formatted-members))
        (dolist (element value)
          (push (json-ppa--format-element-at-depth
                 element (1- depth) base-col)
                formatted-members)))
      (concat (if is-object "{" "[") "\n"
              (mapconcat #'identity (nreverse formatted-members) ",\n")
              "\n" close-indent (if is-object "}" "]"))))))

(defun json-ppa--format-element-at-depth (element depth base-col)
  "Format an array ELEMENT at DEPTH, indented relative to BASE-COL."
  (let* ((member-indent (make-string (+ base-col 2) ?\s))
         (val-str   (json-ppa--format-value-at-depth element depth (+ base-col 2)))
         (val-lines (split-string val-str "\n")))
    (if (= (length val-lines) 1)
        (concat member-indent val-str)
      (mapconcat #'identity
                 (cons (concat member-indent (string-trim-left (car val-lines)))
                       (cdr val-lines))
                 "\n"))))

(defun json-ppa--format-member-at-depth (key value depth base-col)
  "Format object member KEY: VALUE at DEPTH, indented relative to BASE-COL."
  (let* ((member-indent (make-string (+ base-col 2) ?\s))
         (key-str   (format "%s%s: " member-indent (json-ppa--encode (symbol-name key))))
         (val-str   (json-ppa--format-value-at-depth value depth (+ base-col 2)))
         (val-lines (split-string val-str "\n")))
    (if (= (length val-lines) 1)
        (concat key-str val-str)
      (mapconcat #'identity
                 (cons (concat key-str (string-trim-left (car val-lines)))
                       (cdr val-lines))
                 "\n"))))

(defun json-ppa--apply (beg end base-col collection depth)
  "Replace region BEG..END with COLLECTION formatted at DEPTH from BASE-COL."
  (let ((formatted (json-ppa--format-value-at-depth collection depth base-col)))
    (delete-region beg end)
    (goto-char beg)
    (insert formatted)))

;;; Internal helpers — JSON-alike cleanup

(defun json-ppa--cleanup-text (text)
  "Preprocess TEXT from a JSON-alike document to produce valid JSON.

Handles, in order:
0a. Mid-token line wraps are joined without a space: \"conf\\nirmed\" → \"confirmed\".
    Matches a word character at end of line followed by a word character or
    quote at the start of the next line.
0b. All remaining newlines (soft-wrap between tokens) are collapsed to a space.
1. Control characters U+0000-U+001F (except \\t \\n \\r), U+FEFF (UTF-8 BOM),
   and U+FFFD (Unicode replacement character) are removed — JSON forbids
   unescaped control characters, and the latter two are encoding artefacts.
2. Spaced-out uppercase identifiers are collapsed: \"A L W A Y S\" → \"ALWAYS\".
   Matches sequences of uppercase letters, digits, and underscores each
   separated by exactly one space.
3. Single-quoted strings: \\='TEXT\\=' → \"TEXT\"
4. Unquoted identifier keys: { key: → { \"key\":
5. Double-outer-quoted strings: \"\"TEXT\"\" → \"TEXT\", trimming any
   surrounding whitespace left by steps 1-2.
6. Key/value split across lines (colon at end of line): join onto one line.
7. Extra whitespace between a key's closing quote and the colon.
8. Multiple spaces after a colon collapsed to a single space."
  ;; 0a. Join mid-token line wraps without a space
  ;;     "conf\nirmed" → "confirmed",  av\n"ailable" → av"ailable"
  (setq text (replace-regexp-in-string
              "\\([[:alnum:]_]\\)\n\\([[:alnum:]_\"]\\)"
              "\\1\\2" text))
  ;; 0b. Collapse remaining newlines (between-token soft wraps) to a space
  (setq text (replace-regexp-in-string "\n[ \t]*" " " text))
  ;; 1. Remove control chars, BOM (U+FEFF), and replacement char (U+FFFD)
  (setq text (replace-regexp-in-string
              "[\x00-\x08\x0b\x0c\x0e-\x1f\ufeff\ufffd]" "" text))
  ;; 2. Collapse spaced-out identifiers: "A L W A Y S _ L I S T E N I N G" → "ALWAYS_LISTENING"
  (setq text (replace-regexp-in-string
              "[A-Z0-9_]\\(?: [A-Z0-9_]\\)+"
              (lambda (m) (replace-regexp-in-string " " "" m))
              text))
  ;; 3. 'TEXT' → "TEXT"  (single-quoted strings to double-quoted)
  (setq text (replace-regexp-in-string "'\\([^']*\\)'" "\"\\1\"" text))
  ;; 4. { identifierKey: → { "identifierKey":
  ;;    Match a word identifier preceded by {, comma, or whitespace and
  ;;    followed by optional whitespace + colon.  Skip already-quoted keys.
  (setq text (replace-regexp-in-string
              "\\([{,[:space:]]\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\([ \t]*:\\)"
              "\\1\"\\2\"\\3" text))
  ;; 5. ""TEXT"" → "TEXT", trimming whitespace at the edges of the content
  ;;    (the leading space left by step 1 when a control char preceded the text)
  (setq text (replace-regexp-in-string
              "\"\"\\([^\"]*\\)\"\""
              (lambda (m) (concat "\"" (string-trim (match-string 1 m)) "\""))
              text))
  ;; 6. colon at end-of-line, value on next line → join (no-op after 0b)
  (setq text (replace-regexp-in-string ":\\s-*\n\\s-*" ": " text))
  ;; 7. "key"   : → "key":
  (setq text (replace-regexp-in-string "\"[ \t]+:" "\":" text))
  ;; 8. :   value → : value  (two or more spaces collapsed to one)
  (setq text (replace-regexp-in-string ":[ \t][ \t]+" ": " text))
  text)

;;; Commands

;;;###autoload
(defun json-pretty-print-members ()
  "Fully pretty print the JSON array or object at point.
All nested collections are expanded with indentation derived from the
column of the opening bracket."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds)) (end (cdr bounds))
           (base-col   (save-excursion (goto-char beg) (current-column)))
           (collection (json-ppa--read-collection-at beg end)))
      (json-ppa--apply beg end base-col collection most-positive-fixnum))))

;;;###autoload
(defun json-compact-members ()
  "Format the JSON collection at point: one element/member per line, minified.
Each element (array) or member value (object) is collapsed to a single line."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds)) (end (cdr bounds))
           (base-col   (save-excursion (goto-char beg) (current-column)))
           (collection (json-ppa--read-collection-at beg end)))
      (json-ppa--apply beg end base-col collection 1))))

;;;###autoload
(defun json-format-to-depth (depth)
  "Format the JSON collection at point, expanding to DEPTH levels.
Collections shallower than DEPTH are fully expanded; those at or beyond
DEPTH are minified to a single line.
With a numeric prefix argument, use it as DEPTH.  Otherwise prompt."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Format to depth: " 2))))
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds (user-error "No JSON array or object found at point"))
    (let* ((beg (car bounds)) (end (cdr bounds))
           (base-col   (save-excursion (goto-char beg) (current-column)))
           (collection (json-ppa--read-collection-at beg end)))
      (json-ppa--apply beg end base-col collection depth))))

;;;###autoload
(defun json-cleanup-at-point ()
  "Clean up a JSON-alike document at point and reformat it.

Fixes common formatting issues before parsing:
- Control characters (U+0000–U+001F) stripped from string content
- Single-quoted strings: \\='TEXT\\=' → \"TEXT\"
- Unquoted identifier keys: { key: → { \"key\":
- Double-outer-quoted strings: \"\"TEXT\"\" → \"TEXT\"
- Key and value split across lines (colon at end of line)
- Extra whitespace before and after colons

After cleanup the result is parsed and fully pretty-printed.
On parse failure the cleaned text is written to *Messages* for inspection."
  (interactive)
  (let ((bounds (json-ppa--collection-bounds)))
    (unless bounds (user-error "No JSON collection found at point"))
    (let* ((beg     (car bounds))
           (end     (cdr bounds))
           (base-col (save-excursion (goto-char beg) (current-column)))
           (cleaned (json-ppa--cleanup-text
                     (buffer-substring-no-properties beg end)))
           (collection
            (condition-case err
                (let ((json-array-type  'list)
                      (json-object-type 'alist)
                      (json-null        :json-null))
                  (json-read-from-string cleaned))
              (error
               (message "json-cleanup: cleaned text was:\n%s" cleaned)
               (user-error "JSON parse error after cleanup: %s"
                           (error-message-string err))))))
      (json-ppa--apply beg end base-col collection most-positive-fixnum))))

(provide 'json-pretty-print-array)
;;; json-pretty-print-array.el ends here
