;;; json-pretty-print-array-test.el --- ERT tests for json-pretty-print-array  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q \
;;     -L ~/.emacs.d/site-lisp/json-pretty-print-array \
;;     -l json-pretty-print-array-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json-pretty-print-array)

;;; Helper macro

(defmacro with-json-buffer (content &rest body)
  "Execute BODY with a temporary buffer containing CONTENT.
Point is placed at the beginning of the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body
     (buffer-string)))

;;; json-pretty-print-members — arrays

(ert-deftest json-pretty-print-members-single-element ()
  (should (equal
           (with-json-buffer "[1]"
             (json-pretty-print-members))
           "[\n  1\n]")))

(ert-deftest json-pretty-print-members-multiple-numbers ()
  (should (equal
           (with-json-buffer "[1,2,3]"
             (json-pretty-print-members))
           "[\n  1,\n  2,\n  3\n]")))

(ert-deftest json-pretty-print-members-nested-objects ()
  (should (equal
           (with-json-buffer "[{\"a\":1},{\"b\":2}]"
             (json-pretty-print-members))
           "[\n  {\n    \"a\": 1\n  },\n  {\n    \"b\": 2\n  }\n]")))

(ert-deftest json-pretty-print-members-already-pretty ()
  (should (equal
           (with-json-buffer "[\n  1,\n  2\n]"
             (json-pretty-print-members))
           "[\n  1,\n  2\n]")))

(ert-deftest json-pretty-print-members-mixed-types ()
  ;; null is a scalar; with json-null sentinel it encodes back to "null"
  (should (equal
           (with-json-buffer "[\"hello\",42,true,null]"
             (json-pretty-print-members))
           "[\n  \"hello\",\n  42,\n  true,\n  null\n]")))

;;; json-pretty-print-members — objects

(ert-deftest json-pretty-print-members-object ()
  (should (equal
           (with-json-buffer "{\"a\":1,\"b\":2}"
             (json-pretty-print-members))
           "{\n  \"a\": 1,\n  \"b\": 2\n}")))

(ert-deftest json-pretty-print-members-nested-object ()
  (should (equal
           (with-json-buffer "{\"x\":{\"y\":1}}"
             (json-pretty-print-members))
           "{\n  \"x\": {\n    \"y\": 1\n  }\n}")))

;;; json-pretty-print-members — at point (inside nested structure)

(ert-deftest json-pretty-print-members-at-point-inner ()
  "Cursor inside inner array at column 1; indentation is relative to that column."
  (should (equal
           (with-json-buffer "[[1,2],[3,4]]"
             (goto-char 2)          ; inside first inner array at col 1
             (json-pretty-print-members))
           "[[\n   1,\n   2\n ],[3,4]]")))

(ert-deftest json-pretty-print-members-surrounded-by-text ()
  "Indentation is relative to the column of the opening bracket (col 6)."
  (should (equal
           (with-json-buffer "prefix[1,2]suffix"
             (goto-char 8)
             (json-pretty-print-members))
           "prefix[\n        1,\n        2\n      ]suffix")))

;;; json-pretty-print-members — column-aware indentation

(ert-deftest json-pretty-print-members-column-aware ()
  "Indentation is relative to the column of the opening bracket."
  (should (equal
           (with-json-buffer "    [1,2]"
             (goto-char 5)
             (json-pretty-print-members))
           "    [\n      1,\n      2\n    ]")))

(ert-deftest json-pretty-print-members-object-column-aware ()
  (should (equal
           (with-json-buffer "  {\"a\":1}"
             (goto-char 3)
             (json-pretty-print-members))
           "  {\n    \"a\": 1\n  }")))

;;; json-minimize-members — arrays

(ert-deftest json-minimize-members-simple ()
  (should (equal
           (with-json-buffer "[{\"a\":1},{\"b\":2}]"
             (json-minimize-members))
           "[\n  {\"a\":1},\n  {\"b\":2}\n]")))

(ert-deftest json-minimize-members-nested-objects ()
  "Each top-level element (an object with a nested object) becomes one line."
  (should (equal
           (with-json-buffer "[{\"k\":\"A\",\"v\":{\"x\":1}},{\"k\":\"B\",\"v\":{\"x\":2}}]"
             (json-minimize-members))
           "[\n  {\"k\":\"A\",\"v\":{\"x\":1}},\n  {\"k\":\"B\",\"v\":{\"x\":2}}\n]")))

(ert-deftest json-minimize-members-single-element ()
  (should (equal
           (with-json-buffer "[{\"a\":1}]"
             (json-minimize-members))
           "[\n  {\"a\":1}\n]")))

(ert-deftest json-minimize-members-scalars ()
  (should (equal
           (with-json-buffer "[1,2,3]"
             (json-minimize-members))
           "[\n  1,\n  2,\n  3\n]")))

;;; json-minimize-members — objects

(ert-deftest json-minimize-members-object ()
  "Object members: each value is minified."
  (should (equal
           (with-json-buffer "{\"a\":{\"x\":1},\"b\":{\"y\":2}}"
             (json-minimize-members))
           "{\n  \"a\": {\"x\":1},\n  \"b\": {\"y\":2}\n}")))

;;; json-minimize-members — at point

(ert-deftest json-minimize-members-at-point ()
  "Cursor inside inner array at column 1; indentation relative to that column."
  (should (equal
           (with-json-buffer "[[1,2],[3,4]]"
             (goto-char 2)
             (json-minimize-members))
           "[[\n   1,\n   2\n ],[3,4]]")))

;;; json-format-to-depth

(ert-deftest json-format-to-depth-1-equals-minimize ()
  "depth=1 is equivalent to json-minimize-members."
  (should (equal
           (with-json-buffer "[{\"a\":1},{\"b\":2}]"
             (json-format-to-depth 1))
           "[\n  {\"a\":1},\n  {\"b\":2}\n]")))

(ert-deftest json-format-to-depth-large-equals-pretty ()
  "A very large depth expands everything, same as json-pretty-print-members."
  (should (equal
           (with-json-buffer "[{\"a\":1}]"
             (json-format-to-depth 99))
           "[\n  {\n    \"a\": 1\n  }\n]")))

(ert-deftest json-format-to-depth-0-minifies-all ()
  "depth=0 minifies the entire collection."
  (should (equal
           (with-json-buffer "[{\"a\":1},{\"b\":2}]"
             (json-format-to-depth 0))
           "[{\"a\":1},{\"b\":2}]")))

(ert-deftest json-format-to-depth-2-nested-array ()
  "depth=2 on a feature-flag array: outer array and each element object are
expanded, but each element's member values are minified."
  (let ((input  "[{\"key\":\"A\",\"value\":{\"enabled\":true}},{\"key\":\"B\",\"value\":{\"enabled\":false}}]")
        (expect "[\n  {\n    \"key\": \"A\",\n    \"value\": {\"enabled\":true}\n  },\n  {\n    \"key\": \"B\",\n    \"value\": {\"enabled\":false}\n  }\n]"))
    (should (equal
             (with-json-buffer input
               (json-format-to-depth 2))
             expect))))

(ert-deftest json-format-to-depth-3-deeply-nested ()
  "depth=3 expands outer array, element objects, and their nested values."
  (let ((input  "[{\"key\":\"A\",\"value\":{\"enabled\":true}}]")
        (expect "[\n  {\n    \"key\": \"A\",\n    \"value\": {\n      \"enabled\": true\n    }\n  }\n]"))
    (should (equal
             (with-json-buffer input
               (json-format-to-depth 3))
             expect))))

;;; Error cases

(ert-deftest json-pretty-print-members-no-collection ()
  "Error is signalled when there is no array or object at point."
  (should-error
   (with-json-buffer "plain text here"
     (json-pretty-print-members))
   :type 'user-error))

(ert-deftest json-minimize-members-no-collection ()
  (should-error
   (with-json-buffer "no json here"
     (json-minimize-members))
   :type 'user-error))

(provide 'json-pretty-print-array-test)
;;; json-pretty-print-array-test.el ends here

;;; json-cleanup-at-point

(ert-deftest json-cleanup-extra-spaces-around-colon ()
  "Extra whitespace around colons is normalized."
  (should (equal
           (with-json-buffer "{\"a\"    :    \"b\"}"
             (json-cleanup-at-point))
           "{\n  \"a\": \"b\"\n}")))

(ert-deftest json-cleanup-key-value-split-across-lines ()
  "Colon at end-of-line with value on next line is joined."
  (should (equal
           (with-json-buffer "{\"a\"    :\n    \"b\"}"
             (json-cleanup-at-point))
           "{\n  \"a\": \"b\"\n}")))

(ert-deftest json-cleanup-double-outer-quotes ()
  "Double-outer-quoted strings \"\"TEXT\"\" are unwrapped to \"TEXT\"."
  (should (equal
           (with-json-buffer "{\"name\"    :    \"\"SPEECH\"\"}"
             (json-cleanup-at-point))
           "{\n  \"name\": \"SPEECH\"\n}")))

(ert-deftest json-cleanup-full-sample ()
  "Full JSON-alike sample with all three issues is cleaned and reformatted."
  (let ((input "{
    \"speech\"    :
    {
        \"name\"    :    \"\"SPEECH\"\",
        \"enabled\"    :    \"1\"
    }
}"))
    (should (equal
             (with-json-buffer input
               (json-cleanup-at-point))
             "{\n  \"speech\": {\n    \"name\": \"SPEECH\",\n    \"enabled\": \"1\"\n  }\n}"))))
