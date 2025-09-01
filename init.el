;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix; lexical-binding: t; -*-
;;; Commentary:
;; Load all configuration parts

(message "** Init entered")

;; useful for quickly debugging Emacs
(setq debug-on-error t)
;; (setenv "LSP_USE_PLISTS" "true")
;;; Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))


;; TODO:
;;  Check this init
;; https://github.com/alexmurray/dot_emacs.d/blob/master/init.el
;; https://github.com:jcs-emacs/jcs-emacs.git

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

(defgroup kb-config nil
  "Custom options for KB config."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/lollinus/my-emacs-config"))

;; =========================== leaf bootstrap ==========================
;; <leaf-install-code>
(message "** leaf bootstrap begin")
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>
;; =========================== leaf bootstrap ==========================

;; Now you can use leaf!
;; (leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

(message "load path %S" load-path)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Basics
(leaf window
  :doc "GNU Emacs window commands aside from those written in C"
  :tag "builtin" "internal"
  :added "2025-09-01"
  :bind (("M-o" . other-window))
;;  :init (put 'other-window 'repeat-map nil)
)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :added "2025-09-01"
  :custom ((kill-read-only-ok . t)
           ;; (global-mark-ring-max . 5000)         ; increase mark ring to contains 5000 entries
           ;; (mark-ring-max . 5000)                ; increase kill ring to contains 5000 entries
           ;; (kill-ring-max . 5000) ; increase kill-ring capacity
           (kill-whole-line . t)
           ;; (transient-mark-mode . t)
           (indent-tabs-mode . nil)
           ;; (eval-expression-print-length . nil)
           ;; (eval-expression-print-level . nil)
	   )

  :custom
  ;; Hide commands in M-x which do not apply to the current
  ;; mode. Vertico and Corfu commands are hidden, since they are not used
  ;; via M-x. This setting is useful beyond Corfu and Vertico.
  (read-extended-command-predicate . #'command-completion-default-include-p)

  :bind (("C-;" . kill-whole-line)
         ("C-j" . kb/join-line))
  :preface
  ;; Join lines as in Vim
  (defun kb/join-line()
    "Join current and next line.

     Remove tralinig spaces leaving only one.  Similar to Vim Ctrl-j."
    (interactive)
    (join-line 'forward-line))

  :global-minor-mode (column-number-mode line-number-mode size-indication-mode)
  :config
  (put 'set-goal-column 'disabled nil)

  ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
  (if (version< emacs-version "30.1")
      (setopt idle-update-delay 1.0)  ; default is 0.5
    (setopt which-func-update-delay 1.0))
  )


(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "files" "convenience" "emacs>=28.1"
  :url "https://github.com/minad/vertico"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :global-minor-mode t
  :bind (vertico-map
         ;; Option 1: Additional bindings
         ("?"        . #'minibuffer-completion-help)
;;         ("M-RET"    . #'minibuffer-force-complete-and-exit)
;;         ("M-TAB"    . #'minibuffer-complete)
;;         ("\177"     . vertico-directory-delete-char)
;;         ("<return>" . vertico-directory-enter)
         ;; ("/"        . jcs-vertico-/)
         ;; (":"        . jcs-vertico-:)

         ;; Option 2: Replace `vertico-insert' to enable TAB prefix expansion.
         ;; (keymap-set vertico-map "TAB" #'minibuffer-complete)
         )
  ;; :custom (
  ;; 	   (vertico-cycle . t)
  ;;          (vertico-resize . t)
  ;;          (vertico-scroll-margin . 0)
  ;;          (vertico-preselect . 'first)
  ;;          )


  ;; do not consider case significant in completion (GNU Emacs default)
  ;; (setq completion-ignore-case t
  ;;       read-file-name-completion-ignore-case t
  ;;       read-buffer-completion-ignore-case t)

  :config
  (message "**** Configure vertico")
  (vertico-mouse-mode +1)
  )

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "files" "matching" "emacs>=28.1"
  :url "https://github.com/minad/consult"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :bind
  (("M-s r" . consult-ripgrep)))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :added "2025-09-01"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode prescient-persist-mode
  ;; :config
  ;; (prescient-persist-mode)
)

(leaf vertico-prescient
  :doc "Prescient.el + Vertico"
  :req "emacs-27.1" "prescient-6.1.0" "vertico-0.28" "compat-29.1"
  :tag "extensions" "emacs>=27.1"
  :url "https://github.com/radian-software/prescient.el"
  :added "2025-09-01"
  :emacs>= 27.1
  :ensure t
  :global-minor-mode t)

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-28.1" "compat-30"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :after compat)

(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-28.1" "compat-30" "embark-1.1" "consult-1.8"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-09-01"
  :emacs>= 28.1
  :bind ((minibuffer-mode-map
         :package emacs
         ("M-." . embark-dwim)
         ("C-." . embark-act))))


;; Development
(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-28.1" "compat-30.1" "cond-let-0.1" "llama-1.0" "magit-section-4.3.8" "seq-2.24" "transient-0.9.3" "with-editor-3.4.4"
  :tag "vc" "tools" "git" "emacs>=28.1"
  :url "https://github.com/magit/magit"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :after compat cond-let llama magit-section with-editor)

;; Other
(leaf zerodark-theme
  :doc "A dark, medium contrast theme for Emacs"
  :req "all-the-icons-2.0.0"
  :tag "themes"
  :url "https://github.com/NicolasPetton/zerodark-theme"
  :added "2025-09-01"
  :ensure t
  :after all-the-icons)

(leaf circadian
  :doc "Theme-switching based on daytime"
  :req "emacs-27.2"
  :tag "themes" "emacs>=27.2"
  :url "https://github.com/GuidoSchmidt/circadian"
  :added "2025-09-01"
  :emacs>= 27.2
  :ensure t
  :hook ((emacs-startup-hook . circadian-setup)
         (circadian-after-load-theme-hook .
                                          (lambda (theme)
                                            ;; Line numbers appearance
                                            (setq linum-format 'linum-format-func)
                                            ;; Cursor
                                            (set-default 'cursor-type 'box)
                                            (set-cursor-color "#F52503"))))
  :custom
  (calendar-latitude . 53.51)
  (calendar-longitude . 14.57)
  (circadian-themes . '((:sunrise . deeper-blue)
                        (:sunset . tsdh-dark)
                        ("8:00" . tango-dark)
                        ("8:15" . zerodark)
                        ("15:00" . (modus-vivendi))
                        ("15:15" . wombat)
                        ("17:00" . wheatgrass)
                        ("21:30" . leuven-dark)))
  )

(message "** Init finished")
;;; init.el ends here
