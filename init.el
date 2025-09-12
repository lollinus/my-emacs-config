;;; package --- emacs initialization script; -*- mode: emacs-lisp; coding: utf-8-unix; lexical-binding: t; -*-
;;; Commentary:
;; Load all configuration parts

(message "** Init entered")

;; useful for quickly debugging Emacs
(setq debug-on-error t)
;; (setenv "LSP_USE_PLISTS" "true")
;;; Startup
(add-hook 'emacs-startup-hook
          (lambda () (message "*** Emacs loaded in %s seconds with %d garbage collections." (emacs-init-time "%.2f") gcs-done)))

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

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Basics
(leaf window
  :doc "GNU Emacs window commands aside from those written in C"
  :tag "builtin" "internal"
  :added "2025-09-01"
  :bind (("M-o" . other-window)))

(leaf frame
  :doc "multi-frame management independent of window systems"
  :tag "builtin" "internal"
  :added "2025-09-01"
  :config
  (blink-cursor-mode -1))

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
  (defun kb/join-line()
    "Join current and next line.

     Remove tralinig spaces leaving only one.  Similar to Vim Ctrl-j."
    (interactive)
    (join-line 'forward-line))

  :global-minor-mode ((column-number-mode line-number-mode size-indication-mode) . simple)
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
           (history-length . 1000)))

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

(leaf indent
  :doc "indentation commands for Emacs"
  :tag "builtin"
  :added "2025-09-02"
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent . 'complete))

(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :added "2025-09-04"
  :custom (display-line-numbers-width-start . t)
  :hook ((emacs-lisp-mode-hook
          lisp-mode-hook
          c-mode-common-hook
          c-ts-base-mode-hook prog-mode-hook)
         . display-line-numbers-mode))


;;--------------------------------------------------------------------------------
;; iBuffer
;;--------------------------------------------------------------------------------
(leaf ibuffer
  :doc "operate on buffers like dired"
  :tag "builtin"
  :added "2025-09-11"
  :bind (("C-x C-b" . ibuffer))
  :custom ((ibuffer-saved-filter-groups .
                                        '(("default"
                                           ("Emacs Configuration" (or (filename . ".emacs.d")
                                                                      (filename . "init.el")
                                                                      (filename . "package.el")
                                                                      (filename . "private.el")
                                                                      (filename . "emacs.d")))
                                           ("Org" (or (mode . org-mode)
                                                      (filename . "OrgMode")))
                                           ("Magit" (name . "magit"))
                                           ("Help" (or (name . "*Help*")
                                                       (name . "*Apropos*")
                                                       (name . "*info*")))
                                           ("Dired" (mode . dired-mode))
                                           ;; Dev has groups for all languages you program in
                                           ("Dev" (or (mode . cc-mode)
                                                      (filename . ".c")
                                                      (filename . ".h")
                                                      (filename . ".cpp")
                                                      (filename . ".hpp")
                                                      (filename . ".java")
                                                      (filename . ".properties")
                                                      (filename . ".gradle")
                                                      (filename . ".am")
                                                      (mode . yaml-ts-mode)
                                                      (mode . yaml-mode)
                                                      (mode . yang-mode)
                                                      (mode . protobuf-mode)
                                                      (mode . prog-mode)
                                                      (mode . c-ts-mode)
                                                      )
                                            )
                                           ("Text" (or (filename . ".csv")
                                                       (filename . ".tsv")
                                                       (filename . ".txt")
                                                       (filename . ".log")
                                                       (filename . ".json")
                                                       (filename . ".md")))
                                           ("Emacs" (or (name . "^\\*scratch\\*$")
                                                        (name . "^\\*Messages\\*$")))
                                           ("Gnus" (or (mode . message-mode)
                                                       (mode . bbdb-mode)
                                                       (mode . mail-mode)
                                                       (mode . gnus-group-mode)
                                                       (mode . gnus-summary-mode)
                                                       (mode . gnus-article-mode)
                                                       (name . "^\\.bbdb$")
                                                       (name . "^\\.newsrc-dribble"))))))
           (ibuffer-formats . '((mark modified read-only " "
                                      (name 40 40 :left :elide)
                                      " "
                                      (mode 16 16 :left :elide)
                                      " "
                                      filename-and-process))))
  :preface
  (defun kb/ibuffer-switch-to-filter () (ibuffer-switch-to-saved-filter-groups "default"))
  :hook (ibuffer-mode-hook . kb/ibuffer-switch-to-filter))

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
  ;;       (vertico-cycle . t)
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
   ;; GOTO
   ([remap imenu] . consult-imenu) ;; "M-g i"
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flymake
   ([remap goto-line] . consult-goto-line)  ;; "M-g g" orig. goto-line
   ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
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
  :after compat
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("M-." . embark-dwim)        ;; good alternative: M-.
         ([remap describe-bindings] . embark-bindings)  ;; "C-h B" alternative for `describe-bindings'
         ("<f2> i" . #'embark-info-lookup-symbol)
         ("<f2> u" . #'embark-save-unicode-character))
  :custom
  (prefix-help-command . #'embark-prefix-help-command)
  )

(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-28.1" "compat-30" "embark-1.1" "consult-1.8"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :bind ((minibuffer-mode-map
          :package emacs
          ("M-." . embark-dwim)
          ("C-." . embark-act)))
  )

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :req "emacs-28.1" "compat-30"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/corfu"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle . t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary . nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match . nil)      ;; Never quit, even if there is no match
  (corfu-preview-current . nil)    ;; Disable current candidate preview
  (corfu-preselect . 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match . nil)     ;; Configure handling of exact matches
  (corfu-auto . t)
  (corfu-auto-prefix . 2)
  (corfu-auto-delay . 0.1)
  (corfu-echo-delay . 0.25)
  (corfu-popupinfo-delay . '(0.25 . 0.1))
  (corfu-popupinfo-hide . nil)

  :hook (eshell-mode-hook . (lambda () (setq-local corfu-quit-at-boundary t
                                              corfu-quit-no-match t
                                              corfu-auto nil)
                              (corfu-mode)))
  :bind ((:corfu-map
          ("M-SPC" . corfu-insert-separator)
          ("RET" . nil) ; Leave my enter alone!
          ("TAB" . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ("M-RET" . corfu-insert)
          ("S-RET" . corfu-insert)
          ("C-RET" . corfu-insert)))
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  ;; Enable optional extension modes:
  :global-minor-mode (global-corfu-mode corfu-history-mode corfu-popupinfo-mode)
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

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "help" "docs" "emacs>=28.1"
  :url "https://github.com/minad/marginalia"
  :added "2025-09-02"
  :emacs>= 28.1
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         (:minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  ;; The :init configuration is always executed (Not lazy!)
  :custom (marginalia-align . 'right)
  :global-minor-mode t)

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
  :ensure t
  :commands (project-magit)
  :bind (project-prefix-map
         :package project
         ("m" . project-magit))
  :preface
  (defun project-magit ()
   (interactive)
   (let ((dir (project-root (project-current t))))
     (magit-status dir)))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(project-magit "Magit" m))))

(leaf git-timemachine
  :doc "Walk through git revisions of a file"
  :req "emacs-24.3" "transient-0.1.0"
  :tag "vc" "emacs>=24.3"
  :url "https://codeberg.org/pidu/git-timemachine"
  :added "2025-09-01"
  :emacs>= 24.3
  :ensure t)

(leaf git-link
  :doc "Get the GitHub/Bitbucket/GitLab URL for a buffer location"
  :req "emacs-24.3"
  :tag "convenience" "azure" "aws" "sourcehut" "gitlab" "bitbucket" "github" "vc" "git" "emacs>=24.3"
  :url "http://github.com/sshaw/git-link"
  :added "2025-09-01"
  :emacs>= 24.3
  :ensure t)

(leaf project
  :doc "Operations on the current project"
  :tag "builtin"
  :added "2025-09-01"
  :bind (("M-s M-s" . project-find-file)
         (project-prefix-map
          ("d" . project-dired)
          ("M-s" . project-find-file)))
  :init
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-dired "Dired")
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

(leaf breadcrumb
  :doc "Project and imenu-based breadcrumb paths"
  :req "emacs-28.1" "project-0.9.8"
  :tag "emacs>=28.1"
  :url "https://github.com/joaotavora/breadcrumb"
  :added "2025-09-01"
  :emacs>= 28.1
  :ensure t
  :global-minor-mode t
  )

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :tag "builtin"
  :added "2025-09-01"
  :hook (((c-mode-common-hook c-ts-base-mode-hook) . eglot-ensure)
         (eglot-mode-hook . eglot-inlay-hints-mode)
         )
  :custom ((eglot-send-changes-idle-time . 0.1)
           (eglot-extend-to-xref . t)              ; activate Eglot in referenced non-project files
           )
  :bind ((:eglot-diagnostics-map
          ("M-RET" . eglot-code-actions))
           (:eglot-mode-map ("C-c e r" . 'eglot-rename)
                            ("C-c e o" . 'eglot-code-action-organize-imports)
                            ("C-c e h" . 'eldoc)
                            ("<f6>" . 'xref-find-definitions)
                            ("C-c e a" . 'eglot-code-actions)))
  ;; ;; :init
  ;; ;; (eval-after-load 'eglot
  ;; ;;   (define-key global-map (kbd "C-c e") 'eglot-mode-map))
  ;; (eglot-inlay-hints-mode)
  )

(leaf consult-eglot
  :doc "A consulting-read interface for eglot"
  :req "emacs-27.1" "eglot-1.16" "consult-0.31" "project-0.3.0"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/mohkale/consult-eglot"
  :added "2025-09-04"
  :emacs>= 27.1
  :ensure t
  :bind ("M-s s" . #'consult-eglot-symbols))

(leaf consult-eglot-embark
  :doc "Embark integration for `consult-eglot'"
  :req "emacs-27.1" "consult-eglot-0.3" "embark-consult-1.0"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/mohkale/consult-eglot"
  :added "2025-09-04"
  :emacs>= 27.1
  :ensure t
  :after consult-eglot
  :global-minor-mode t)

;; Put credentials in ~/.authinfo.gpg enctyped file and set them for packages needing below snippet
;; (require 'auth-source)
;; (let* ((auth (car (auth-source-search
;; 	       :host "jira.cc.bmwgroup.net"
;; 	       :requires '(:user :secret))))
;;    (password (funcall (plist-get auth :secret)))
;;    (username (plist-get auth :user)))
;;   (message "creds %S USR: %S AUTH: %S" `(:username ,(plist-get auth :user) :password ,password) username auth))

(leaf clang-format
  :doc "Format code using clang-format."
  :req "cl-lib-0.3"
  :tag "c" "tools"
  :url "https://github.com/emacsmirror/clang-format"
  :added "2022-10-31"
  :ensure t
  :when (executable-find "clang-format")
  ;; :bind ((:c-mode-base-map
  ;;         ("C-M-'" . #'clang-format-region))
  ;;        (:c++-mode-map
  ;;         ("C-M-'" . #'clang-format-region))
  ;;        (:c-mode-map
  ;;         ("C-M-'" . #'clang-format-region)))
  :preface
  (defun kb/c++-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c++-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c++-ts-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c++-ts-mode-map "C-M-'" #'clang-format-region))
  (defun kb/c-ts-bind-clang-format ()
    "Hook used to bind clang-format on moded activation."
    ;; (interactive)
    ;; (define-key c-mode-base-map (kbd "C-M-'") 'clang-format-region)
    (keymap-set c-ts-mode-map "C-M-'" #'clang-format-region))
  :hook (c-mode-hook . kb/c-bind-clang-format )
  :hook (c++-mode-hook . kb/c++-bind-clang-format)
  :hook (c-ts-mode-hook . kb/c-ts-bind-clang-format)
  :hook (c++-ts-mode-hook . kb/c++-ts-bind-clang-format)
  )

(leaf verb
  :doc "Organize and send HTTP requests"
  :req "emacs-26.3"
  :tag "tools" "emacs>=26.3"
  :url "https://github.com/federicotdn/verb"
  :added "2025-09-01"
  :emacs>= 26.3
  :ensure t
  :after org
  ;; :bind-keymap (:org-mode-map ("C-c C-1" . verb-command-map))
  :config
  (add-to-list 'org-babel-load-languages '((verb . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; (define-key org-mode-map (kbd "C-c C-0") verb-command-map)
  (keymap-set org-mode-map "C-c C-0" verb-command-map)
  )

(leaf highlight-doxygen
  :doc "Highlight Doxygen comments"
  :tag "faces"
  :url "https://github.com/Lindydancer/highlight-doxygen"
  :added "2025-09-05"
  :ensure t
  :hook (c-mode-common-hook c-ts-base-mode-hook))

(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :req "cl-lib-0.2" "emacs-26.1"
  :tag "diff" "vc" "emacs>=26.1"
  :url "https://github.com/dgutov/diff-hl"
  :added "2025-09-11"
  :emacs>= 26.1
  :ensure t
  :global-minor-mode global-diff-hl-mode global-diff-hl-amend-mode
  :hook ((dired-mode-hook . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom ((diff-hl-draw-borders . t)
           ;; PERF: Slightly more conservative delay before updating the diff
           (diff-hl-flydiff-delay . 0.5)  ; default: 0.3
           ;; PERF: don't block Emacs when updating vc gutter
           ;;
           ;; NOTE: Async feature is buggy for now.
           (diff-hl-update-async . nil)
           ;; UX: get realtime feedback in diffs after staging/unstaging hunks
           ;; (diff-hl-show-staged-changes . nil)
           )
  )

(leaf highlight-indent-guides
  :doc "Minor mode to highlight indentation"
  :req "emacs-26.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :added "2025-09-12"
  :emacs>= 26.1
  :ensure t
  :global-minor-mode t
  :init
  (setq highlight-indent-guides-method 'character
        ;; highlight-indent-guides-character (elenv-choose-char ?\┊ ?\|)
        highlight-indent-guides-character ?\┊
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))

(leaf so-long
  :doc "Say farewell to performance problems with minified code."
  :tag "builtin"
  :added "2025-07-18"
  :custom
  (so-long-threshold . 5000)
  :config
  (nconc so-long-minor-modes
         '( eldoc-mode
            highlight-numbers-mode
            highlight-indent-guides-mode
            hl-fill-column-mode
            line-reminder-mode
            page-break-lines-mode
            tree-sitter-mode
            ts-fold-mode ts-fold-indicators-mode
            lsp-mode eglot--managed-mode
            whitespace-cleanup-mode))
  :global-minor-mode global-so-long-mode)

;; Documents

(leaf org
  :doc "Outline-based notes management and organizer"
  :tag "builtin"
  :added "2025-09-02"
  :ensure t
  :custom ((org-startup-indented . t)
           (org-startup-with-inline-images . t)
           (org-pretty-entities . t)
           (org-use-sub-superscripts . "{}")
           (org-hide-emphasis-markers . t)
           (org-hide-block-startup . t)
           (org-image-actual-width . '(300))

           (org-ellipsis . " ▾")
           (org-hide-leading-stars . t)
           (org-agenda-start-with-log-mode . t)
           (org-log-done . 'time)
           (org-log-into-drawer . t)
           (org-src-fontify-natively . t)
           (org-list-allow-alphabetical . t)

           (org-export-default-language . "en")
           (org-export-with-fixed-width . nil)
           (org-export-preserve-breaks . t)
           (org-export-with-properties . t)
           (org-export-with-section-numbers . nil)
           (org-export-with-smart-quotes . t)
           (org-export-with-drawers . nil)
           (org-export-with-todo-keywords . nil)
           (org-export-with-broken-links . t)
           (org-export-with-toc . nil)
           (org-export-with-smart-quotes . t)
           (org-export-date-timestamp-format . "%d %B %Y")
           (org-export-with-sub-superscripts . '{})
           (org-export-copy-to-kill-ring . 'if-interactive)
           (org-export-show-temporary-export-buffer . nil)

           (org-html-table-default-attributes . '(:border "1" :rules "border" :frame "all"))
           (org-html-metadata-timestamp-format . "%A, %B %d, %Y")
           (org-html-postamble . nil)
           (org-todo-keywords . '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                                  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
           )

  :preface
  (defun kb/org-mode-setup ()
    (org-indent-mode 1)
    ;;(variable-pitch-mode 1)
    (visual-line-mode 1)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (eshell . t)
       (screen . t)
       (shell . t)
       (dot . t)
       (gnuplot . t)
       )))
  :hook ((org-mode-hook . kb/org-mode-setup))

  :preface
  (defun ews-org-insert-screenshot ()
    "Take a screenshot with ImageMagick and insert as an Org mode link."
    (interactive)
    (let ((filename (read-file-name "Enter filename for screenshot: " default-directory)))
      (unless (string-equal "png" (file-name-extension filename))
        (setq filename (concat (file-name-sans-extension filename) ".png")))
      (call-process-shell-command (format "import %s" filename))
      (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
      (insert (format "[[file:%s]]" filename))
      (org-redisplay-inline-images)))
  :bind (org-mode-map ("C-<print>" . ews-org-insert-screenshot))
 )

(leaf ox-jira
  :doc "JIRA Backend for Org Export Engine"
  :req "org-8.3"
  :tag "wp" "hypermedia" "outlines"
  :url "https://github.com/stig/ox-jira.el"
  :added "2025-09-02"
  :ensure t
  :after ox
  :require t)

(leaf ox-gfm
  :doc "Github Flavored Markdown Back-End for Org Export Engine"
  :tag "github" "markdown" "wp" "org"
  :url "https://github.com/larstvei/ox-gfm"
  :added "2025-09-11"
  :ensure t
  :after ox
  :require t)

;; (require 'auth-source)
;; (let* ((auth (car (auth-source-search
;; 	       :host "jira.cc.bmwgroup.net"
;; 	       :requires '(:user :secret))))
;;    (password (funcall (plist-get auth :secret)))
;;    (username (plist-get auth :user)))
;;   (message "creds %S USR: %S AUTH: %S" `(:username ,(plist-get auth :user) :password ,password) username auth))

(leaf org-jira
  :doc "Syncing between Jira and Org-mode"
  :req "emacs-24.5" "cl-lib-0.5" "request-0.2.0" "dash-2.14.1"
  :tag "tracker" "bug" "org" "jira" "ahungry" "emacs>=24.5"
  :url "https://github.com/ahungry/org-jira"
  :added "2025-09-02"
  :emacs>= 24.5
  :ensure t
  :after org
  :config
  (when (not (file-exists-p "~/.org-jira"))
    (make-directory "~/.org-jira"))
  (setopt jiralib-use-restapi t)
  (setopt jiralib-url "https://jira.cc.bmwgroup.net")
  (let* ((auth (car (auth-source-search
 	       :host "jira.cc.bmwgroup.net"
 	       :requires '(:user :secret))))
    (password (funcall (plist-get auth :secret)))
    (username (plist-get auth :user))
    (jira-host (plist-get auth :host)))
    (setopt jiralib-host jira-host)
    (setopt jiralib-user username)
    (setopt jiralib-token `("Authorization" . ,(concat "Bearer " password))))
  (setopt jiralib-use-PAT t))

(leaf gt
  :doc "Translation framework, configurable and scalable"
  :req "emacs-28.1" "pdd-0.2.3"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/lorniu/gt.el"
  :added "2025-09-04"
  :emacs>= 28.1
  :ensure t
  :custom ((gt-langs . '(en de ja ko pt pl fr zh)))
  :config
  (setq gt-default-translator
         (gt-translator
          :taker (gt-taker :text 'buffer :pick 'paragraph :langs '(en de ja ko pt pl fr zh))
          :engines (list (gt-google-engine) (gt-bing-engine) (gt-deepl-engine))
          :render (gt-buffer-renderer)))

  (setq gt-preset-translators
      `((ts-quick-de-en . ,(gt-translator
                  :taker (gt-taker :langs '(de en) :text 'word)
                  :engines (gt-bing-engine)
                  :render (gt-overlay-render)))
        (ts-quick-en-pl . ,(gt-translator
                            :taker (gt-taker :langs '(en pl) :text 'word)
                            :engines (gt-deepl-engine)
                            :render (gt-overlay-render)))
        (ts-quick-ja-en . ,(gt-translator
                            :taker (gt-taker :langs '(ja en) :text 'word)
                            :engines (gt-bing-engine)
                            :render (gt-overlay-render)))
        (ts-in-place . ,(gt-translator
                         :taker (gt-taker :langs '(en de ja ko) :text 'sentence)
                         :engines (gt-google-engine)
                         :render (gt-insert-render)))
        (ts-3 . ,(gt-translator
                  :taker (gt-taker :langs '(en de) :text 'buffer
                                   :pick 'word :pick-pred (lambda (w) (length> w 6)))
                  :engines (gt-google-engine)
                  :render (gt-overlay-render :type 'help-echo)))))
  :bind (("<f5>" . #'gt-translate)))

(leaf hl-todo
  :doc "Highlight TODO and similar keywords"
  :req "emacs-26.1" "compat-30.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/tarsius/hl-todo"
  :added "2025-09-09"
  :emacs>= 26.1
  :ensure t
  ;; :bind ((:hl-todo-mode-map
  ;;         ("C-c t p" . #'hl-todo-previous)
  ;;         ("C-c t n" . #'hl-todo-next)
  ;;         ("C-c t o" . #'hl-todo-occur)
  ;;         ("C-c t i" . #'hl-todo-insert)))
  ;; :hook prog-mode-hook
  :config
  (add-to-list 'hl-todo-keyword-faces '("NOCOMMIT" . "#ff00ff"))
  :global-minor-mode global-hl-todo-mode)

;; Other
(leaf comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(leaf iedit
  :doc "Edit multiple regions in the same way simultaneously"
  :tag "refactoring" "simultaneous" "region" "occurrence"
  :url "https://github.com/victorhge/iedit"
  :added "2025-09-03"
  :ensure t
  :bind ("C-c ;" . iedit-mode))

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

(leaf kill-file-path
  :doc "Copy file name into kill ring"
  :req "emacs-26"
  :tag "files" "emacs>=26"
  :url "https://github.com/chyla/kill-file-path/kill-file-path.el"
  :added "2025-09-02"
  :emacs>= 26
  :ensure t
  ;; :bind ("C-c f" . kill-file-path)
  )

(message "** Init finished")
;;; init.el ends here
