;;; rc-ruby.el ---
;; ruby mode configuration file

; (add-to-list 'load-path
;              (concat my-site-lisp-directory "ruby-mode"))

; (if (or (string-equal (getenv "OSTYPE") "Linux")
;         (string-equal (getenv "OSTYPE") "linux")
;         (string-equal (getenv "OSTYPE") "linux-gnu"))
;     (setq ruby-program-name "/usr/bin/ruby")
;   (or (string-equal (getenv "OSTYPE") "FreeBSD" ))
;   (setq ruby-program-name "/usr/local/bin/ruby"))
; (autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
; (setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))


; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process")
; (autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

; (add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
; (add-hook 'ruby-mode-hook '(lambda () (setq show-trailing-whitespace t)))
; (add-hook 'ruby-mode-hook 'font-lock-fontify-numbers)

; (require 'ruby-electric)

;;; rc-ruby.el ends here
