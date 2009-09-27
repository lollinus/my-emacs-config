(codepage-setup '1250)
(define-coding-system-alias 'windows-1250 'cp1250)
(defun ogonki (arg) ""
  (let ((coding-system-for-read 'windows-1250)
        (coding-system-for-write 'iso-latin-2))
    (message (format "Converting file: '%s'" arg))
    (find-file arg)
    (write-file (format "%s-iso8859-2" arg))))