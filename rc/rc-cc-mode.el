;;; rc-c-mode.el --- My C mode setup
;;; Commentary:
;; This is init setup file for C mode
;;; Code:
;;-------------------------------------------------------------------------------
;; indentation styles
;;-------------------------------------------------------------------------------

(require 'cc-mode)

;;   Linux Kernel code
(defun c-lineup-arglist-tabs-only (ignored)
  "IGNORED Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Add kernel style
(c-add-style
 "linux-tabs-only"
 '("linux" (c-offsets-alist
            (arglist-cont-nonempty
             c-lineup-gcc-asm-reg
             c-lineup-arglist-tabs-only))))

;; My favorite C++ style
(c-add-style "my-bsd-style"
	     '("bsd"
	       (c-basic-offset . 4)
	       (tab-width . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist
		(inline-open . 0)
		(innamespace . 0)
		)))

;;-------------------------------------------------------------------------------
;; styl indentacji kodu
;;-------------------------------------------------------------------------------
(setq c-default-style
      '((c++-mode . "my-bsd-style")
	(c-mode . "linux")
	(other . "bsd")))

(add-hook 'c-mode-common-hook
	  (lambda ()
	     (c-toggle-hungry-state 0)
	     (setq show-trailing-whitespace t)
	     ;; display line numbers
	     (setq display-line-numbers 'visual)
	     (display-line-numbers-mode t)
	     )
	  )
(add-hook 'c-mode-common-hook 'font-lock-fontify-numbers)

;; If makefile doesn't exist compile with g++ -Wall -o <current file name> <current file name>
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "g++")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq fill-column 80)
	    (fci-mode)
	    (auto-fill-mode)
	    (setq tab-width 8)
	    (setq indent-tabs-mode t)
	    ))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq fill-column 80)
	    (auto-fill-mode)
	    (fci-mode)
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)
	    ))

(provide 'rc-cc-mode)

;;; rc-cc-mode.el ends here
