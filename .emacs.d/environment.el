;; -*- mode: lisp; coding: utf-8-unix -*-
;;
;; This file is part of emacs initialization files
;; It is repsonsible to setup runtime environment for emacs
;;


;; OS type -- are we running Microsoft Windows?
(defvar running-ms-windows (eq system-type 'windows-nt))

; Set PATH depending on operating system being run
; (getenv "PATH")
(if running-ms-windows
    (setenv "PATH"
            (concat
             "C:\\Karol\\Programy\\GnuWin32\\bin" ";"
             "C:\\Karol\\Programy\\doxygen\\bin" ";"
             (getenv "PATH")
             )
            )
  )

