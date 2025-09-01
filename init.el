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

;; Faster to disable these here (before they've been initialized)
(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars)
         (fullscreen . maximized))
       (when (featurep 'ns)
         '((ns-transparent-titlebar . t)))
       default-frame-alist))

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

(leaf frame
  :doc "multi-frame management independent of window systems"
  :tag "builtin" "internal"
  :added "2025-09-01")

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

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :added "2025-09-01"
  :custom ((user-full-name . "Karol Barski")
           (user-mail-address . "karol.barski@cognizant.com")
           (user-login-name . "karolbarski")
           (create-lockfiles . nil)
           (init-file-debug . t)
           (truncate-lines . t)
           (tab-width . 4)                       ; default to 4 visible spaces to display a tab
           (menu-bar-mode . nil)
           (tool-bar-mode . nil)
           ;; Resizing the Emacs frame can be a terribly expensive part of changing the
           ;; font. By inhibiting this, we halve startup times, particularly when we use
           ;; fonts that are larger than the system default (which would resize the frame).
           (frame-inhibit-implied-resize . t)
           (frame-resize-pixelwise . t)
           ;; Support opening new minibuffers from inside existing minibuffers.
           (enable-recursive-minibuffers . t)
           (history-length . 1000)
  ))

(leaf time
  :doc "display time, load and mail indicator in mode line of Emacs"
  :tag "builtin"
  :added "2025-09-01"
  :custom ((display-time-format . "%H:%M %d/%m/%Y")
           (display-time-24hr-format . t)
           (display-time-day-and-date . t)
           (display-time-default-load-average . nil) ; this information is useless for most
           )
  :global-minor-mode display-time-mode)

(leaf ffap
  :doc "find file (or url) at point"
  :tag "builtin"
  :added "2025-09-01"
  :init
  (ffap-bindings)
  :config
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

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
  (("M-s r" . consult-ripgrep)
   ([remap project-switch-to-buffer] . consult-project-buffer) ;; "C-x p b" orig. project-switch-to-buffer
   ("M-s l" . consult-line)
   ("C-s" . kb/consult-line) ;; "C-s" Isearch
   ([remap imenu] . consult-imenu) ;; "M-g i"
   )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :preface
  (defun kb/consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :custom
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key . "C-+") ;; "<"

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

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
         ("C-." . embark-act)))
)

(leaf cape
  :doc "Completion At Point Extensions"
  :req "emacs-28.1" "compat-30"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/cape"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  :config
  ;; Make capfs composable
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)

  ;; Example 6: Define interactive Capf which can be bound to a key.  Here we wrap
  ;; the `elisp-completion-at-point' such that we can complete Elisp code
  ;; explicitly in arbitrary buffers.
  (keymap-global-set "C-c a e" (cape-capf-interactive #'elisp-completion-at-point))

  (when (< emacs-major-version 29)
    ;; Silence then pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; Ensure that pcomplete does not write to the buffer and behaves as
    ;; pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  )

;; Development

;; Show symbols as composed characters (e.g. lambda -> λ)
(leaf prog-mode
  :doc "Generic major mode for programming"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :global-minor-mode global-prettify-symbols-mode)

(leaf c-ts-mode
  :doc "tree-sitter support for C and C++"
  :tag "builtin"
  :added "2025-09-01"
  :custom ((c-ts-common-indent-offset . 4)
           (c-ts-mode-enable-doxygen . t)
           (c-ts-mode-indent-offset . 4))
  :preface
  (defun kb/c++-setup-symbol-compose ()
    "Define additional symbol composition rules for C++ mode."
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    )

  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'auto-mode-alist
               '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
                 . c++-ts-mode))

  :config
  (c-ts-mode-set-global-style 'linux)
  (defun kb/c++-ts-mode-hook ()
    "My style used while editing C++ sources."
    (setq-local fill-column 120)
    ;; (tab-width . 4)
    (indent-tabs-mode . nil)
    (display-fill-column-indicator-mode)
    (kb/c++-setup-symbol-compose)
    )
  :hook ((c-ts-mode-hook c++-ts-mode-hook) . kb/c++-ts-mode-hook)
  )

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-28.1" "compat-30.1" "cond-let-0.1" "llama-1.0" "magit-section-4.3.8" "seq-2.24" "transient-0.9.3" "with-editor-3.4.4"
  :tag "vc" "tools" "git" "emacs>=28.1"
  :url "https://github.com/magit/magit"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t)

(leaf project
  :doc "Operations on the current project"
  :tag "builtin"
  :added "2025-09-01"
  :bind (("M-s M-s" . project-find-file)
         (:project-prefix-map
          ("m" . project-magit)
          ("d" . project-dired)
          ("M-s" . project-find-file)))
  :init
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-dired "Dired")
            (project-magit "Magit")
            (project-compile "Compile")
            (project-find-regexp "Find Regex")
            (eat-project "Terminal"))
          project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :config
  (when (>= emacs-major-version 30)
    (setopt project-mode-line t))
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-function
        (lambda (may-prompt)
          (when-let* ((project (project-current))
                      (project-root (car (project-roots (project-current))))
                      (is-not-home
                       (not (string= "/home/karolbarski/" (car (project-roots
                                                                (project-current)))))))
            project-root)))

  ;; Added in emacs 29
  (setopt project-vc-extra-root-markers
          '("*.cabal" "pyproject.toml"
            "CMakeList.txt"
            "package.json"
            "Project.toml" ".project"
            "Cargo.toml""mix.exs" "qlfile" ".git"))
  (setopt project-vc-ignores (append project-vc-ignores
                                     '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git"
                                       ".hg" ".fslockout" "_FOSSIL_" ".bzr" "_darcs" ".tox"
                                       ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd")
                                     '(".log" ".vs" "node_modules")
                                     ))

  )

(leaf consult-project-extra
  :doc "Consult integration for project.el"
  :req "emacs-27.1" "consult-0.17" "project-0.8.1"
  :tag "management" "project" "convenience" "emacs>=27.1"
  :url "https://github.com/Qkessler/consult-project-extra"
  :added "2025-09-01"
  :emacs>= 27.1
  :ensure t
  :bind
  ([remap project-find-file] . consult-project-extra-find)
  )

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :tag "builtin"
  :added "2025-09-01"
  :hook (((c-mode-common-hook c-ts-base-mode-hook) . eglot-ensure))
  :custom ((eglot-send-changes-idle-time . 0.1)
           (eglot-extend-to-xref . t)              ; activate Eglot in referenced non-project files
           )
  ;; :bind ((:eglot-diagnostics-map
  ;;         ("M-RET" . eglot-code-actions))
  ;;        (:eglot-mode-map ("C-c e r" . 'eglot-rename)
  ;;                         ("C-c e o" . 'eglot-code-action-organize-imports)
  ;;                         ("C-c e h" . 'eldoc)
  ;;                         ("<f6>" . 'xref-find-definitions)
  ;;                         ("C-c e a" . 'eglot-code-actions)))
  ;; ;; :init
  ;; ;; (eval-after-load 'eglot
  ;; ;;   (define-key global-map (kbd "C-c e") 'eglot-mode-map))
  ;; (eglot-inlay-hints-mode)
  )


;; Other
(leaf comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((projectile-project-test-cmd
      . "docker run --rm --user 1001:1002 -v /home/karolbarski/.hmi-build-home/:/home/karolbarski -w /home/karolbarski/projects/ID8/ --mount type=bind,source=/home/karolbarski/projects/ID8/,target=/home/karolbarski/projects/ID8/ hmi-user-builder ./build/x64_linux64_hmi8mgu22_debug/install/bin/speech_speechdevices_gtest")
     (projectile-project-test-dir
      . "./build/x64_linux64_hmi8mgu22_debug/install/bin")
     (projectile-project-compilation-cmd
      . "docker run --rm --user 1001:1002 -v /home/karolbarski/.hmi-build-home/:/home/karolbarski -w /home/karolbarski/projects/ID8/ --mount type=bind,source=/home/karolbarski/projects/ID8/,target=/home/karolbarski/projects/ID8/ hmi-user-builder ./build/x64_linux64_hmi8mgu22_debug/run_build.sh -j8")
     (projectile-project-configure-cmd
      . "docker run --rm --user 1001:1002 -v /home/karolbarski/.hmi-build-home/:/home/karolbarski -w /home/karolbarski/projects/ID8/ --mount type=bind,source=/home/karolbarski/projects/ID8/,target=/home/karolbarski/projects/ID8/ --env MAKETHREADCOUNT=8 hmi-user-builder ./build_linux64mgu22.sh")
     (projectile-project-compilation-dir . "../")
     (projectile-project-name . "HMI/speech")
     (projectile-project-configure-cmd
      . "docker run --rm --user 1001:1002 -v /home/karolbarski/.hmi-build-home/:/home/karolbarski -w /home/karolbarski/projects/ID8/ --mount type=bind,source=/home/karolbarski/projects/ID8/,target=/home/karolbarski/projects/ID8/ --env MAKETHREADCOUNT=8 MGU22/hmi-user-builder:0.1-dev ./build_linux64mgu22.sh")
     (projectile-project-name . "ID8")
     (projectile-project-compilation-cmd
      . "docker run --rm --user 1001:1002 -v /home/karolbarski/.hmi-build-home/:/home/karolbarski -w /home/karolbarski/projects/ID8/ --mount type=bind,source=/home/karolbarski/projects/ID8/,target=/home/karolbarski/projects/ID8/ --env MAKETHREADCOUNT=8 MGU22/hmi-user-builder:0.1-dev ./build/x64_linux64_hmi8mgu22_debug/run_build.sh -j10")
     (projectile-project-compilation-dir . "./"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
