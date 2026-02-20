;;; json-pretty-print-array-test.el --- Tests for json-pretty-print-array  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `json-pretty-print-array-elements' and
;; `json-pretty-print-array-elements-minimize'.
;;
;; Run from the command line:
;;   emacs --batch -Q -L ~/.emacs.d/site-lisp/json-pretty-print-array \
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

;;; Tests for `json-pretty-print-array-elements-minimize' — arrays

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
  "Pretty-printed array input: each element collapsed to one indented line."
  (let* ((input "[  {\n    \"key\": \"WAKEUP_WORD\",\n    \"value\": true\n  },\n  {\n    \"key\": \"WEATHER\",\n    \"value\": false\n  }\n]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (lines (split-string result "\n" t)))
    (should (string= (car lines) "["))
    (should (string= (car (last lines)) "]"))
    (dolist (line (butlast (cdr lines)))
      (should (string-prefix-p "  " line)))))

(ert-deftest json-ppa-minimize-preserves-element-count ()
  "Minified array output contains the same number of elements as the input."
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
  "Nested objects inside array are minified to a single indented line."
  (let* ((input "[{\"key\":\"K\",\"value\":{\"name\":\"K\",\"enabled\":true}}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (inner (cadr (split-string result "\n"))))
    (should (string-prefix-p "  " inner))
    (should (string-match-p "\"enabled\":true" inner))
    (should (not (string-match-p "\n" inner)))))

(ert-deftest json-ppa-minimize-point-inside-array ()
  "Function operates on array when point is between its elements."
  (let ((result (with-temp-buffer
                  (insert "[{\"a\":1},{\"b\":2}]")
                  (goto-char 9)               ; point at ',' between elements
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
                  (goto-char (+ (length "before\n") 1))
                  (json-pretty-print-array-elements-minimize)
                  (buffer-string))))
    (should (string-prefix-p "before\n" result))
    (should (string-suffix-p "\nafter" result))
    (should (string-match-p "\\[" result))
    (should (string-match-p "\"a\":1" result))))

;;; Tests for `json-pretty-print-array-elements-minimize' — objects

(ert-deftest json-ppa-minimize-object-scalar-values ()
  "Object with scalar values: each member on one indented line."
  (let* ((result (with-json-buffer "{\"a\":1,\"b\":true}"
                   (json-pretty-print-array-elements-minimize)))
         (lines (split-string result "\n")))
    (should (string= (car lines) "{"))
    (should (string= (car (last lines)) "}"))
    (should (seq-some (lambda (l) (string-match-p "\"a\": *1" l)) lines))
    (should (seq-some (lambda (l) (string-match-p "\"b\": *true" l)) lines))))

(ert-deftest json-ppa-minimize-object-nested-value ()
  "Object with nested object value: inner value minified to one line."
  (let* ((input "{\"cfg\":{\"x\":1,\"y\":2},\"name\":\"test\"}")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (lines (split-string result "\n" t)))
    ;; cfg member line should contain the minified nested object
    (should (seq-some (lambda (l) (string-match-p "\"cfg\".*{\"x" l)) lines))
    ;; that line must not itself contain newlines
    (let ((cfg-line (seq-find (lambda (l) (string-match-p "\"cfg\"" l)) lines)))
      (should cfg-line)
      (should (not (string-match-p "\n" cfg-line))))))

(ert-deftest json-ppa-minimize-object-preserves-member-count ()
  "Minimized object output has same number of members as input."
  (let* ((input "{\"a\":1,\"b\":2,\"c\":3}")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (parsed (let ((json-object-type 'alist))
                   (json-read-from-string result))))
    (should (= (length parsed) 3))))

(ert-deftest json-ppa-minimize-object-roundtrip-values ()
  "Values are preserved after object minimize."
  (let* ((input "{\"enabled\":true,\"count\":42}")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements-minimize)))
         (parsed (let ((json-object-type 'alist))
                   (json-read-from-string result))))
    (should (eq (alist-get 'enabled parsed) t))
    (should (= (alist-get 'count parsed) 42))))

;;; Tests for `json-pretty-print-array-elements' — arrays

(ert-deftest json-ppa-pretty-output-starts-with-bracket ()
  "Pretty array output starts with '[\\n'."
  (let ((result (with-json-buffer "[{\"key\":\"A\"}]"
                  (json-pretty-print-array-elements))))
    (should (string-prefix-p "[\n" result))))

(ert-deftest json-ppa-pretty-output-ends-with-bracket ()
  "Pretty array output ends with '\\n]'."
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
  "Two array elements in pretty output are separated by a comma."
  (let* ((result (with-json-buffer "[{\"a\":1},{\"b\":2}]"
                   (json-pretty-print-array-elements)))
         (lines (split-string result "\n")))
    (should (seq-some (lambda (l) (string-suffix-p "," l)) lines))))

(ert-deftest json-ppa-pretty-preserves-element-count ()
  "Re-parsing pretty array output yields the same number of elements."
  (let* ((input "[{\"a\":1},{\"b\":2},{\"c\":3}]")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements)))
         (parsed (let ((json-array-type 'list)) (json-read-from-string result))))
    (should (= (length parsed) 3))))

(ert-deftest json-ppa-pretty-roundtrip-values ()
  "Array values are preserved after pretty-printing."
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

;;; Tests for `json-pretty-print-array-elements' — objects

(ert-deftest json-ppa-pretty-object-output-braces ()
  "Pretty object output starts with '{\\n' and ends with '\\n}'."
  (let ((result (with-json-buffer "{\"key\":\"A\"}"
                  (json-pretty-print-array-elements))))
    (should (string-prefix-p "{\n" result))
    (should (string-suffix-p "\n}" result))))

(ert-deftest json-ppa-pretty-object-members-indented ()
  "Each object member line is indented by at least 2 spaces."
  (let ((result (with-json-buffer "{\"a\":1,\"b\":2}"
                  (json-pretty-print-array-elements))))
    (dolist (line (split-string result "\n" t))
      (unless (member line '("{" "}"))
        (should (string-prefix-p "  " line))))))

(ert-deftest json-ppa-pretty-object-nested-value-multiline ()
  "Nested object value is pretty-printed across multiple lines."
  (let* ((input "{\"cfg\":{\"x\":1,\"y\":2}}")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements))))
    ;; The result should have more lines than a single-line render
    (should (> (length (split-string result "\n")) 3))))

(ert-deftest json-ppa-pretty-object-roundtrip-values ()
  "Values are preserved after object pretty-print."
  (let* ((input "{\"enabled\":true,\"count\":42,\"name\":\"test\"}")
         (result (with-json-buffer input
                   (json-pretty-print-array-elements)))
         (parsed (let ((json-object-type 'alist))
                   (json-read-from-string result))))
    (should (eq (alist-get 'enabled parsed) t))
    (should (= (alist-get 'count parsed) 42))
    (should (string= (alist-get 'name parsed) "test"))))

;;; Error cases

(ert-deftest json-ppa-minimize-no-collection-signals-error ()
  "Signals `user-error' when point is not inside any JSON collection."
  (with-temp-buffer
    (insert "plain text, no JSON here")
    (goto-char (point-min))
    (should-error (json-pretty-print-array-elements-minimize)
                  :type 'user-error)))

(ert-deftest json-ppa-pretty-no-collection-signals-error ()
  "Signals `user-error' when point is not inside any JSON collection."
  (with-temp-buffer
    (insert "plain text, no JSON here")
    (goto-char (point-min))
    (should-error (json-pretty-print-array-elements)
                  :type 'user-error)))

;;; Run

(provide 'json-pretty-print-array-test)
;;; json-pretty-print-array-test.el ends here
