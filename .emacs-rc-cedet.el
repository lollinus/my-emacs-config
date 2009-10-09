;;; emacs-rc-cedet.el ---

;; Copyright (C) 2003 Alex Ott
;;
;; Author: alexott@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path
             (concat my-site-lisp-directory "cedet-1.0pre6"))
(setq semantic-load-turn-everything-on t)
(load-file "~/site-lisp/cedet-1.0pre6/common/cedet.el")

(require 'bovine-grammar nil t)
(require 'semantic nil t)
(semantic-load-enable-code-helpers)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-summary-mode 1)
(require 'semantic-sb nil t)
(require 'semanticdb nil t)
(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
;;(semantic-load-enable-all-exuberent-ctags-support)

;;(require 'semantic-c)

;; smart complitions
(require 'semantic-ia)

(setq senator-minor-mode-name "SN")
(setq semantic-imenu-auto-rebuild-directory-indexes nil)
;;(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)
(enable-visual-studio-bookmarks)
(global-semantic-tag-folding-mode 1)

;; gcc setup
(require 'semantic-gcc)
;;(semantic-gcc-setup "gcc")

(setq-mode-local c++-mode semanticdb-find-default-throttle
 				 '(project unloaded system recursive))

(global-semanticdb-minor-mode 1)

;;(setq-mode-local c-mode semanticdb-find-default-throttle
;;                 '(project unloaded system recursive))

(require 'eassist)

;; customisation of modes
(defun my-cedet-hook ()
  ;; (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c." 'senator-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;; ;;  (local-set-key "." 'semantic-complete-self-insert)
;; ;; (local-set-key ">" 'semantic-complete-self-insert)
;;   (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-xt" 'eassist-switch-h-cpp)
;;   )
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;
(setq-default semanticdb-default-save-directory "/tmp/semanticdb")

(semantic-add-system-include "/opt/sysincludes/linux64/ix86/sysincludes_1.3/usr/include" 'c++-mode)
;;(semantic-add-system-include "~/include" 'c-mode)
;;(semantic-add-system-include "/usr/include" 'c-mode)
;;(semantic-add-system-include "/usr/include" 'c++-mode)
;;(semantic-add-system-include "/usr/include/c++/3.4.6" 'c++-mode)
;;(semantic-add-system-include "/vobs/api1007/fsapi/rhel4/x86_64/debug/include/" 'c++-mode)
;;(semantic-add-system-include "../include" 'c++-mode)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 10)
 )

(require 'semantic-lex-spp)
(global-ede-mode t)

(ede-cpp-root-project "Planwagen"
                      :name "Planwagen"
                      :file "/vobs/oms/config.txt"
                      :include-path '("/include"
                                      "/SS_Planwagen/CMPlanManager/build"
                                      "/SS_Planwagen/CMPlanManager/include"
                                      "/SS_Planwagen/CMPlanManager/src"
                                      "/SS_PlanwagenBsi/CMPlanBsi/build"
                                      "/SS_PlanwagenBsi/CMPlanBsi/src"
                                      )
                      :system-include-path '("/opt/sysincludes/linux64/ix86/sysincludes_1.3/usr/include"
                                             "/vobs/api1007/fsapi/rhel4/x86_64/release/include"
                                             "/opt/gcc/linux/ix86/gcc_3.4.5-2p2/include/c++/3.4.5/")
                      :spp-table '(("DEF_NAME" . "def_value")
                                   )
                      )

;;; emacs-rc-cedet.el ends here
