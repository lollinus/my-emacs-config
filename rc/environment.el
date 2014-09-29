;; -*- mode: lisp; coding: utf-8-unix -*-
;;
;; This file is part of emacs initialization files
;; It is repsonsible to setup runtime environment for emacs
;;


;; OS type -- are we running Microsoft Windows?
(defvar running-ms-windows (eq system-type 'windows-nt))

; Set PATH depending on operating system being run
(defun getappdata ()
  (getenv "LOCALAPPDATA"))

(if running-ms-windows
    (progn
      (setenv "PATH"
              (concat
               (getappdata) "\\GnuWin32\\bin" ";"
               (getappdata) "\\PortableGit\\bin\\" ";"
               (getappdata) "\\doxygen\\bin" ";"
               "C:\\Program Files\\Java\\jdk1.7.0\\bin" ";"
			   "c:\\Program Files\\cygwin64\\bin" ";"
			   "c:\\Program Files\\cygwin64\\usr\\bin" ";"
               (getenv "PATH")
               )
              )
      (add-to-list 'exec-path (concat (getappdata) "/PortableGit/bin/"))
      (add-to-list 'exec-path (concat (getappdata) "/GnuWin32/bin/"))
      (add-to-list 'exec-path (concat (getappdata) "/doxygen/bin/"))
      (add-to-list 'exec-path "c:/Program Files/cygwin64/bin/")
      (add-to-list 'exec-path "c:/Program Files/cygwin64/usr/bin/")
      (add-to-list 'exec-path "C:/Program Files/Java/jdk1.7.0/bin")
      (add-to-list 'exec-path "c:/Program Files/Racket/")
      )
  )

(if (eq system-type 'gnu/linux)         ; todo recongnize properly our dev server
    (progn
      (setenv "PATH"
              (concat
               "/opt/graphviz/linux/ix86/graphviz_2.16.1/bin" ":"
               "/opt/doxygen/linux/ix86/doxygen_1.5.8-rhel4/bin" ":"
               (getenv "PATH")
               )
              )
      (add-to-list 'exec-path "/opt/graphviz/linux/ix86/graphviz_2.16.1/bin")
      (add-to-list 'exec-path "/opt/doxygen/linux/ix86/doxygen_1.5.8-rhel4/bin")
      )
  )


;; The most important directories are the last!

;; `local-site-lisp-directory' is there so that you have an easy way of
;; installing your own (possibly not distro packaged) Emacs add-ons which
;; are specific to the version of Emacs your running. This keeps your local
;; add-ons apart from distro supplied ones. If your have a `/usr/local'
;; partition, it also means you can do a complete re-install of Emacs (or
;; even your Linux distro) without impacting on stuff you have added by
;; hand.

;; 3.
(if running-ms-windows
    (defvar my-site-lisp-directory (concat (getenv "HOME") "\\.emacs.d\\site-lisp\\")
      "Name of directory where my personal additional Emacs Lisp files reside.")
  (defvar my-site-lisp-directory "~/.emacs.d/site-lisp/"
    "Name of directory where my personal additional Emacs Lisp files reside.")
  )

(if (string-equal "21" (substring emacs-version 0 2))
    (add-to-list 'load-path
                 (concat my-site-lisp-directory "cua")
                 )
  )
