;;; rc-c-mode.el ---
;;-------------------------------------------------------------------------------
;; styl indentacji kodu
;;-------------------------------------------------------------------------------
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "linux")
        (other . "bsd")))

;;-------------------------------------------------------------------------------
;; użycie tabulatora
;;-------------------------------------------------------------------------------
(setq default-tab-width 4)
(setq indent-tabs-mode nil)
(setq c-basic-offset 4)
;; (require 'cc-mode)
;; ;; personal preferences
;; (c-set-offset 'substatement-open 0)
;; (c-set-offset 'case-label '+)
;; (c-set-offset 'arglist-cont-nonempty '+)
;; (c-set-offset 'arglist-intro '+)
;; (c-set-offset 'topmost-intro-cont '+)

(add-hook 'c-mode-hook '(lambda () (setq show-trailing-whitespace t)))
(add-hook 'c++-mode-hook '(lambda () (setq show-trailing-whitespace t)))
(add-hook 'c-mode-hook 'font-lock-fontify-numbers)
(add-hook 'c++-mode-hook 'font-lock-fontify-numbers)

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

;;-------------------------------------------------------------------------------
;; Linux Kernel code indentation style
;;-------------------------------------------------------------------------------
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
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

(custom-set-variables
 '(c-default-style "linux-tabs-only")
)

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq fill-column 80)
	    (column-marker-3 80)
		(auto-fill-mode)
		(setq tab-width 8)
		(setq indent-tabs-mode t)
		(setq c-basic-offset 8)
		))

;;; rc-c-mode.el ends here
