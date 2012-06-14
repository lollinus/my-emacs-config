;;; rc-date-time.el ---

(defun insert/date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beggining) (region-end) )
    )
  (insert (format-time-string "%Y-%m-%d"))
  )

(defun insert/date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2012-06-10T21:11:33+02:00
See: URL `http://en.wikipedia.org/wiki/ISO_8601'
"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beggining) (region-end) )
    )
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

(global-set-key (kbd "C-c C-d") 'insert/date-time)

;;; rc-date-time.el ends here ----
