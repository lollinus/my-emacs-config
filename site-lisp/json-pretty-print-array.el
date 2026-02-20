;;; json-pretty-print-array.el --- Pretty print each element in a JSON array  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides two commands for reformatting a JSON array in the current buffer:
;;
;; `json-pretty-print-array-elements' - Pretty prints each element of the
;; JSON array at point with proper indentation, producing a readable,
;; multi-line representation.
;;
;; `json-pretty-print-array-elements-minimize' - Encodes each element of the
;; JSON array at point as a single-line minified string, producing a compact
;; one-liner-per-element representation.
;;
;; Both commands replace only the array at point, leaving the rest of the
;; buffer untouched.

;;; Code:

(require 'json)

(defun json-ppa--array-bounds ()
  "Return (BEG . END) of the innermost JSON array around point, or nil."
  (save-excursion
    (condition-case nil
        (progn
          (unless (looking-at "\\[")
            (up-list -1 t t)
            (while (not (looking-at "\\["))
              (up-list -1 t t)))
          (let ((start (point)))
            (forward-sexp)
            (cons start (point))))
      (error nil))))

(defun json-ppa--read-array-at (beg end)
  "Parse the JSON array between BEG and END and return it as a list."
  (let ((json-array-type 'list))
    (json-read-from-string (buffer-substring-no-properties beg end))))

;;;###autoload
(defun json-pretty-print-array-elements ()
  "Pretty print each element of the JSON array at point.
Each element is formatted with proper indentation and 2-space base indent."
  (interactive)
  (let ((bounds (json-ppa--array-bounds)))
    (unless bounds
      (user-error "No JSON array found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (json-array (json-ppa--read-array-at beg end))
           (formatted-elements '()))
      ;; Collect pretty-printed elements
      (dolist (element json-array)
        (let ((pretty-json
               (with-temp-buffer
                 (insert (json-encode element))
                 (goto-char (point-min))
                 (json-pretty-print-buffer)
                 ;; Indent each line by 2 spaces
                 (goto-char (point-min))
                 (while (not (eobp))
                   (insert "  ")
                   (forward-line 1))
                 (buffer-substring-no-properties (point-min) (point-max)))))
          (push pretty-json formatted-elements)))
      ;; Replace the array region in place
      (delete-region beg end)
      (goto-char beg)
      (insert "[\n")
      (insert (mapconcat #'identity (nreverse formatted-elements) ",\n"))
      (insert "\n]"))))

;;;###autoload
(defun json-pretty-print-array-elements-minimize ()
  "Pretty print the JSON array at point with each element minified to one line.
The array brackets are formatted across multiple lines, but each element's
content is minimized to a compact single-line representation."
  (interactive)
  (let ((bounds (json-ppa--array-bounds)))
    (unless bounds
      (user-error "No JSON array found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (json-array (json-ppa--read-array-at beg end))
           (formatted-elements '()))
      ;; Collect minimized elements, each indented by 2 spaces
      (dolist (element json-array)
        (push (concat "  " (json-encode element)) formatted-elements))
      ;; Replace the array region in place
      (delete-region beg end)
      (goto-char beg)
      (insert "[\n")
      (insert (mapconcat #'identity (nreverse formatted-elements) ",\n"))
      (insert "\n]"))))

(provide 'json-pretty-print-array)
;;; json-pretty-print-array.el ends here
