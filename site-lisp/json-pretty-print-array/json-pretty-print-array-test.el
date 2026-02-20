;;; json-pretty-print-array-test.el --- Tests for json-pretty-print-array  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `json-pretty-print-array-elements' and
;; `json-pretty-print-array-elements-minimize'.
;;
;; Run from the command line:
;;   emacs --batch -Q -L ~/.emacs.d/site-lisp \
;;         -l json-pretty-print-array-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json-pretty-print-array)

;;; Helpers

(defmacro with-json-buffer (content &rest body)
  "Execute BODY in a temp buffer pre-filled with CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body
     (buffer-string)))

;;; Tests for `json-pretty-print-array-elements-minimize'

(ert-deftest json-ppa-minimize-single-element ()
  "Single-element array is minified with element on its own indented line."
  (let ((result (with-json-buffer "[{\"key\":\"A\",\"value\":1}]"
                  (json-pretty-print-array-elements-minimize))))
    (should (string-prefix-p "[\n" result))
    (should (string-suffix-p "\n]" result))
    (should (string-match-p "  {\"key\":\"A\",\"value\":1}" result))))

(ert-deftest json-ppa-minimize-two-elements ()
  "Two-element array has each element minified on its own indented line."
  (let ((result (with-json-buffer "[{\"key\":\"A\"},{\"key\":\"B\"}]"
                  (json-pretty-print-array-elements-minimize))))
    (should (string-prefix-p "[\n" result))
    (should (string-suffix-p "\n]" result))
    (should (string-match-p "  {\"key\":\"A\"}," result))
    (should (string-match-p "  {\"key\":\"B\"}" result))))

(ert-deftest json-ppa-minimize-pretty-input ()
  "Pretty-printed input: each element collapsed to one indented line."
  (let* ((input "[  {\n    \"key\": \"WAKEUP_WORD\",\n    \"value\": true\n  },\n  {\n    \"key\": \"WEATHER\",\n    \"value\": false\n  }\n]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (lines (split-string result "\n" t)))
    (should (string= (car lines) "["))
    (should (string= (car (last lines)) "]"))
    ;; Each inner line must be a single minified element (no nested newlines)
    (dolist (line (butlast (cdr lines)))
      (should (string-prefix-p "  " line)))))

(ert-deftest json-ppa-minimize-preserves-element-count ()
  "Minified output contains the same number of elements as the input."
  (let* ((input "[{\"a\":1},{\"b\":2},{\"c\":3}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (parsed (let ((json-array-type 'list)) (json-read-from-string result))))
    (should (= (length parsed) 3))))

(ert-deftest json-ppa-minimize-empty-array ()
  "Empty JSON array produces a formatted empty array."
  (let ((result (with-json-buffer "[]"
                  (json-pretty-print-array-elements-minimize))))
    (should (string= result "[\n\n]"))))

(ert-deftest json-ppa-minimize-nested-objects ()
  "Nested objects are minified to a single indented line."
  (let* ((input "[{\"key\":\"K\",\"value\":{\"name\":\"K\",\"enabled\":true}}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (inner (cadr (split-string result "\n"))))
    (should (string-prefix-p "  " inner))
    (should (string-match-p "\"enabled\":true" inner))
    ;; The inner line itself must not contain newlines
    (should (not (string-match-p "\n" inner)))))

(ert-deftest json-ppa-minimize-point-inside-array ()
  "Function operates on array when point is inside it, not at its start."
  (let ((result (with-temp-buffer
                  (insert "[{\"a\":1},{\"b\":2}]")
                  (goto-char 5)               ; point inside the array
                  (json-pretty-print-array-elements-minimize)
                  (buffer-string))))
    (should (string-prefix-p "[\n" result))
    (should (string-suffix-p "\n]" result))
    (should (string-match-p "  {\"a\":1}" result))
    (should (string-match-p "  {\"b\":2}" result))))

(ert-deftest json-ppa-minimize-array-surrounded-by-text ()
  "Only the array at point is replaced; surrounding text is untouched."
  (let ((result (with-temp-buffer
                  (insert "before\n[{\"a\":1}]\nafter")
                  (goto-char (+ (length "before\n") 1)) ; point inside array
                  (json-pretty-print-array-elements-minimize)
                  (buffer-string))))
    (should (string-prefix-p "before\n" result))
    (should (string-suffix-p "\nafter" result))
    (should (string-match-p "\\[" result))
    (should (string-match-p "\"a\":1" result))))

(ert-deftest json-ppa-minimize-no-array-signals-error ()
  "Signals `user-error' when point is not inside a JSON array."
  (with-temp-buffer
    (insert "{\"key\":\"value\"}")
    (goto-char (point-min))
    (should-error (json-pretty-print-array-elements-minimize)
                  :type 'user-error)))

;;; Tests for `json-pretty-print-array-elements'

(ert-deftest json-ppa-pretty-output-starts-with-bracket ()
  "Pretty output starts with '[\\n'."
  (let ((result (with-json-buffer "[{\"key\":\"A\"}]"
                  (json-pretty-print-array-elements))))
    (should (string-prefix-p "[\n" result))))

(ert-deftest json-ppa-pretty-output-ends-with-bracket ()
  "Pretty output ends with '\\n]'."
  (let ((result (with-json-buffer "[{\"key\":\"A\"}]"
                  (json-pretty-print-array-elements))))
    (should (string-suffix-p "\n]" result))))

(ert-deftest json-ppa-pretty-elements-are-indented ()
  "Each element line is indented by at least 2 spaces."
  (let ((result (with-json-buffer "[{\"key\":\"A\",\"value\":1}]"
                  (json-pretty-print-array-elements))))
    (dolist (line (split-string result "\n" t))
      (unless (member line '("[" "]"))
        (should (string-prefix-p "  " line))))))

(ert-deftest json-ppa-pretty-two-elements-separated-by-comma ()
  "Two elements in pretty output are separated by a comma."
  (let* ((result (with-json-buffer "[{\"a\":1},{\"b\":2}]"
                   (json-pretty-print-array-elements)))
         (lines (split-string result "\n")))
    (should (seq-some (lambda (l) (string-suffix-p "," l)) lines))))

(ert-deftest json-ppa-pretty-preserves-element-count ()
  "Re-parsing pretty output yields the same number of elements."
  (let* ((input "[{\"a\":1},{\"b\":2},{\"c\":3}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements)))
         (parsed (let ((json-array-type 'list)) (json-read-from-string result))))
    (should (= (length parsed) 3))))

(ert-deftest json-ppa-pretty-roundtrip-values ()
  "Values are preserved after pretty-printing."
  (let* ((input "[{\"key\":\"WAKEUP_WORD\",\"enabled\":true},{\"key\":\"WEATHER\",\"enabled\":false}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements)))
         (parsed (let ((json-array-type 'list)) (json-read-from-string result))))
    (should (string= (alist-get 'key (car parsed)) "WAKEUP_WORD"))
    (should (eq (alist-get 'enabled (car parsed)) t))
    (should (string= (alist-get 'key (cadr parsed)) "WEATHER"))
    (should (eq (alist-get 'enabled (cadr parsed)) :json-false))))

(ert-deftest json-ppa-pretty-array-surrounded-by-text ()
  "Only the array at point is replaced; surrounding text is untouched."
  (let ((result (with-temp-buffer
                  (insert "before\n[{\"a\":1}]\nafter")
                  (goto-char (+ (length "before\n") 1))
                  (json-pretty-print-array-elements)
                  (buffer-string))))
    (should (string-prefix-p "before\n" result))
    (should (string-suffix-p "\nafter" result))))

(ert-deftest json-ppa-pretty-no-array-signals-error ()
  "Signals `user-error' when point is not inside a JSON array."
  (with-temp-buffer
    (insert "{\"key\":\"value\"}")
    (goto-char (point-min))
    (should-error (json-pretty-print-array-elements)
                  :type 'user-error)))

;;; Run

(provide 'json-pretty-print-array-test)
;;; json-pretty-print-array-test.el ends here
