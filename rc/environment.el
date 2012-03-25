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
    (setenv "PATH"
            (concat
             (getappdata) "\\GnuWin32\\bin" ";"
             (getappdata) "\\PortableGit\\bin\\" ";"
             (getappdata) "\\doxygen\\bin" ";"
             "C:\\Program Files\\Java\\jdk1.7.0\\bin" ";"
             (getenv "PATH")
             )
            )
  )

;; set exec-path for emacs so it follows "PATH"
(if running-ms-windows
    (progn
      (add-to-list 'exec-path (concat (getappdata) "/PortableGit/bin/"))
      (add-to-list 'exec-path (concat (getappdata) "/GnuWin32/bin/"))
      (add-to-list 'exec-path (concat (getappdata) "/doxygen/bin/"))
      (add-to-list 'exec-path "C:/Program Files/Java/jdk1.7.0/bin")
      )
  )
