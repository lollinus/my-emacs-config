;;; what-env.el --- Determine the local environment for later testing.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Charles Curley <ccurley at trib dot com>
;; Keywords: convenience operating system host
;; Created: 1999 01 05

;; Fixed bug revealed by 21.1. 2001 11 03 C^2

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

; This package is for folks who want their .emacs or other elisp code to
; run on multiple computers and multiple operating systems and handle
; local differences. For example, you might want to set your geometry one
; way for a laptop and another for a workstation with a 21" screen.

; The idea was originally from Cristian Ionescu-Idbohrn
; <cristian.ionescu-idbohrn@axis.com>, and contains suggestions from Jari
; Aalto. Thank you both.

; This could be done as a function to eliminate yet another variable, but
; the variable makes for faster testing than executing a function.

; There is no customization.

; To get this to load in your .emacs, use something like:

; (message "loading what-env...")
; (load "what-env")
; (message "loading what-env... done.")

; although that's probably more elaborate than you really need.

; First we determine our host OS.
;; The result here should be one of the following:
;;    "Windows_95", "Windows_NT", "X10_windows", "X11_windows"
;; or something entirely different ;-))

;; begin what-env

(defvar what-env "Spanish_Inquisition"
"A string value which indicates the current windowing environment, one of
\"Windows_95\", \"Windows_NT\", \"X10_windows\", \"X11_windows\",
\"console\", or something nobody expects. If it comes up
\"Spanish_Inquisition\", you have not run logic to set it. If it comes up
nil you are on something completely different.")

;; Part of the process of changing from Emacs version 19 to 20 was to
;; change all win32* variable names to w32*. To allow this code to run on
;; both 19 and 20, a test for either is necessary. That is why I have the
;; phrase "(memq window-system '(win32 w32))" (suggested by Jari Aalto) in
;; there. Grump.

(setq what-env (or
	       (and(eq window-system 'nil)
		   "console")
	       (and (memq window-system '(win32 w32))
		    (or (getenv "OS") "Windows_95"))
	       (and (eq window-system 'x)
		    (concat "X" (number-to-string window-system-version) "_windows"))
	       ))

;; end what-env.