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


;;; Basics
(leaf emacs-basic-settings
  :doc "Basic startup setting for emacs"
  :config
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  )

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
           (user-mail-address . "karol.barski@partner.bmwgroup.com")
           (user-login-name . "qxz2st8")
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
  :custom
  ;; Don't ping things that look like domain names.
  (ffap-machine-p-known . 'reject)
)

(leaf recentf
  :doc "keep track of recently opened files"
  :tag "builtin"
  :added "2025-12-29"
  :global-minor-mode t)



(leaf files
  :doc "file input and output commands"
  :tag "builtin"
  :added "2025-09-17"
  :global-minor-mode auto-save-visited-mode
  ;; ;; According to the POSIX, a line is defined as "a sequence of zero or
  ;; ;; more non-newline characters followed by a terminating newline".
  ;; (require-final-newline . t)
  :custom `(
            ;; (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            ;; (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup")) (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)
            (auto-save-visited-interval . 1)
            ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
            ;; indicates misconfiguration (don't rely on case insensitivity for file names).
            (auto-mode-case-fold . nil))
  :custom ((large-file-warning-threshold . 100000000)
           (mode-require-final-newline . t)      ; add a newline to end of file)
           (make-backup-files . nil))
  :config
  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist (append major-mode-remap-alist '((yaml-mode . yaml-ts-mode)
                                                                (bash-mode . bash-ts-mode)
                                                                (js2-mode . js-ts-mode)
                                                                (typescript-mode . typescript-ts-mode)
                                                                (json-mode . json-ts-mode)
                                                                (css-mode . css-ts-mode)
                                                                (python-mode . python-ts-mode)
                                                                (javascript-mode . js-ts-mode)))))

(leaf battery
  :doc "display battery status information"
  :tag "builtin"
  :added "2023-02-02"
  :config
  (display-battery-mode))

(leaf indent
  :doc "indentation commands for Emacs"
  :tag "builtin"
  :added "2025-09-02"
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent . 'complete))

(leaf whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :tag "builtin"
  :added "2025-09-22"
  :custom (
           ;; (setq whitespace-style '(face trailing lines-tail newline empty indentation big-indent space-before-tab))
           (whitespace-style . '(newline-mark newline))
           (whitespace-line-column . nil)
           (whitespace-display-mappings . '((space-mark 32 [183] [46])
                                            (newline-mark 10 [9166 10])
                                            (tab-mark 9 [9654 9] [92 9]))))
  :bind (("C-c w w" . whitespace-mode))
  :custom-face
  (whitespace-space . '((t (:inherit whitespace-space :foreground "DimGrey" :background nil))))
  (whitespace-newline . '((t (:inherit whitespace-newline :foreground "DimGrey" :background nil))))
  (whitespace-indentation . '((t (:inherit whitespace-indentation :foreground "DimGrey" :background nil))))

  :global-minor-mode (global-whitespace-mode)
  )

(leaf subword
  :doc "Handling capitalized subwords in a nomenclature"
  :tag "builtin"
  :added "2025-09-22"
  :hook c-mode-common-hook c-ts-base-mode-hook)

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
  :preface
  (defun kb/consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :custom
  ;; One column view with annotations
  (completions-format . 'one-column)
  (completions-detailed . t)
  (completions-group . t)
  ;; Allow navigating from the minibuffer
  (minibuffer-visible-completions . t)
  ;; Show completions eagerly and update automatically
  (completion-eager-update . t)
  (completion-eager-display . t)
  ;; Disable noise (inline help also blocks input)
  (completion-show-help . nil)
  (completion-show-inline-help . nil)

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

(leaf consult-dir
  :doc "Insert paths into the minibuffer prompt"
  :req "emacs-26.1" "project-0.3.0" "consult-2.0"
  :tag "convenience" "emacs>=26.1"
 :url "https://github.com/karthink/consult-dir"
  :added "2025-10-20"
  :emacs>= 26.1
  :ensure t
  :after consult
  :bind
  (global-map ("C-x C-d" . consult-dir))
  (minibuffer-local-completion-map ("C-x C-d" . cnosult-dir)
                                   ("C-x C-j" . consult-dir-jump-file)))

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

(leaf editorconfig
  :disabled t
  :doc "EditorConfig Plugin"
  :tag "builtin"
  :added "2025-10-06"
  :custom (editorconfig-trim-whitespaces-mode . t)
  :global-minor-mode editorconfig-mode)


;;; Development

;; Show symbols as composed characters (e.g. lambda -> λ)
(leaf prog-mode
  :doc "Generic major mode for programming"
  :tag "builtin" "internal"
  :added "2022-11-01"
  :global-minor-mode global-prettify-symbols-mode)

(leaf treesit
  :doc "tree-sitter utilities"
  :tag "builtin" "languages" "tree-sitter" "treesit"
  :added "2025-09-17"
  :custom
  (treesit-language-source-alist . '(
                                     (awk "https://github.com/Beaglefoot/tree-sitter-awk")
                                     (bash "https://github.com/tree-sitter/tree-sitter-bash")
                                     (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
                                     (blueprint "https://github.com/huanie/tree-sitter-blueprint")
                                     (c "https://github.com/tree-sitter/tree-sitter-c")
                                     (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
                                     (closure "https://github.com/sogaiu/tree-sitter-clojure")
                                     (cmake "https://github.com/uyha/tree-sitter-cmake")
                                     (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
                                     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                     (css "https://github.com/tree-sitter/tree-sitter-css")
                                     (dart "https://github.com/ast-grep/tree-sitter-dart")
                                     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                     (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
                                     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                                     (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
                                     (go "https://github.com/tree-sitter/tree-sitter-go")
                                     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
                                     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
                                     (heex "https://github.com/phoenixframework/tree-sitter-heex")
                                     (html "https://github.com/tree-sitter/tree-sitter-html")
                                     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                     (json "https://github.com/tree-sitter/tree-sitter-json")
                                     (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
                                     (latex "https://github.com/latex-lsp/tree-sitter-latex")
                                     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
                                     (magik "https://github.com/krn-robin/tree-sitter-magik")
                                     (make "https://github.com/alemuller/tree-sitter-make")
                                     ;; (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
                                     (org "https://github.com/milisims/tree-sitter-org")
                                     (proto "https://github.com/mitchellh/tree-sitter-proto")
                                     (python "https://github.com/tree-sitter/tree-sitter-python")
                                     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                     (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                     (sql "https://github.com/DerekStride/tree-sitter-sql")
                                     (sparql "https://github.com/GordianDziwis/tree-sitter-sparql")
                                     (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                     (wast "https://github.com/wasm-lsp/tree-sitter-wasm" "main" "wast/src")
                                     (wat "https://github.com/wasm-lsp/tree-sitter-wasm" "main" "wat/src")
                                     (wsgl "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
                                     (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
                                     )))

(leaf mason
  :doc "Package managers for LSP, DAP, linters, and more"
  :req "emacs-30.1" "s-1.13.0"
  :tag "installer" "lsp" "tools" "emacs>=30.1"
  :url "https://github.com/deirn/mason.el"
  :added "2025-10-13"
  :emacs>= 30.1
  :ensure t
  :config
  (mason-ensure))

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
  :mode ("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'" . c++-ts-mode)

  :config
  (c-ts-mode-set-global-style 'linux)
  (defun kb/c++-ts-mode-hook ()
    "My style used while editing C++ sources."
    (message "**** kb/c++-ts-mode-hook")
    (kb/c++-setup-symbol-compose)
    (setq-local fill-column 120)
    ;; (tab-width . 4)
    (indent-tabs-mode -1)
    (display-fill-column-indicator-mode)
    )
  (defun kb/whitespace-progmode-setup ()
    "Enable whitespace mode for programming modes."
    (setq-local whitespace-style
                '(face
                  trailing
                  lines-tail
                  newline
                  empty
                  indentation
                  space-after-tab::tab
                  ;; space-after-tab
                  ;; big-indent
                  space-before-tab::tab
                  ;; space-before-tab
                  tab-mark
                  newline-mark
                  ))
    )
  :hook ((c-ts-mode-hook c++-ts-mode-hook) . kb/c++-ts-mode-hook)
  ((c-ts-mode-hook c++-ts-mode-hook) . kb/whitespace-progmode-setup)
  )

(leaf haskell-mode
  :doc "A Haskell editing mode"
  :req "emacs-25.1"
  :tag "languages" "repl" "ghc" "cabal" "haskell" "emacs>=25.1"
  :url "https://github.com/haskell/haskell-mode"
  :added "2025-10-14"
  :emacs>= 25.1
  :ensure t
  :mode "\\.hs\\'"
  :bind (:haskell-mode-map
         ("C-," . #'haskell-move-nested-left)
         ("C-." . #'haskell-move-nested-right)
         ("<f7>" . #'haskell-navigate-imports)
         ("C-c C-g" . #'haskell-interactive-bring)
         ("C-c C-l" . #'haskell-process-load-or-reload)
         ("C-c C-t" . #'haskell-process-do-type)
         ("C-c C-i" . #'haskell-process-do-info)
         ("C-c C-c" . #'haskell-process-cabal-build)
         ("C-c C-k" . #'haskell-interactive-mode-clear)
         ("C-c c" . #'haskell-process-cabal)
         ("M-n" . haskell-goto-next-error)
         ("M-p" . haskell-goto-prev-error)
         ;; ("<SPC>" . #'haskell-mode-contextual-space)
         )
  :custom ((haskell-stylish-on-save . t)
           (haskell-process-type . 'cabal-repl)
           (haskell-process-suggest-remove-import-lines . t)
           (haskell-process-auto-import-loaded-modules . t)
           (haskell-process-log . t)
           )
  :hook ((haskell-mode-hook . interactive-haskell-mode)
         (haskell-mode-hook . turn-on-haskell-doc-mode)
         (haskell-mode-hook . haskell-indent-mode)
         (haskell-mode-hook . kb/haskell-setup-outline-mode))
  :config
  (defun kb/haskell-setup-outline-mode ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-"))
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
                 '(project-magit "Magit" m)))
  :config
  (add-to-list 'magit-process-find-password-functions 'magit-process-password-auth-source))

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
                                     )))

(leaf consult-project-extra
  :doc "Consult integration for project.el"
  :req "emacs-27.1" "consult-0.17" "project-0.8.1"
  :tag "management" "project" "convenience" "emacs>=27.1"
  :url "https://github.com/Qkessler/consult-project-extra"
  :added "2025-09-01"
  :emacs>= 27.1
  :ensure t
  :custom (consult-project-function . #'consult-project-extra-project-fn)
  :bind
  ([remap project-find-file] . consult-project-extra-find))

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
  :preface
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun kb/eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode-hook . kb/eglot-eldoc)))

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

(leaf symbols-outline
  :doc "Display symbols (functions, variables, etc) in outline view"
  :req "emacs-27.1"
  :tag "outlines" "emacs>=27.1"
  :url "https://github.com/liushihao456/symbols-outline.el"
  :added "2025-10-06"
  :emacs>= 27.1
  :ensure t
  :bind ("C-c i" . #'symbols-outline-show)
  :custom ((symbols-outline-fetch-fn . #'symbols-outline-lsp-fetch)
           (symbols-outline-window-position . 'left))
  :global-minor-mode symbols-outline-follow-mode)

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :tag "builtin"
  :added "2025-10-06"
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . scan-buf-next-region)
         ("M-g M-p" . scan-buf-previous-region)
         ;; (:flymake-repeatmap
         ;;  ("p" . scan-buf-previous-region)
         ;;  ("n" . scan-buf-next-region)
         ;;  ("M-p" . scan-buf-previous-region)
         ;;  ("M-n" . scan-buf-next-region))
         (flymake-diagnostics-buffer-mode-map
          ("?" . kb/flymake-show-diagnostic-here))
         (flymake-project-diagnostics-mode-map
          ("?" . kb/flymake-show-diagnostic-here))
         (prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))
         )
  :hook (prog-mode-hook . flymake-mode)
  :preface
  (defun kb/flymake-show-diagnostic-here (pos &optional other-window)
    "Show the full diagnostic of this error.

Used to see multiline flymake errors"
    (interactive (list (point) t))
    (let* ((id (or (tabulated-list-get-id pos)
                   (user-error "Nothing at point")))
           (text (flymake-diagnostic-text (plist-get id :diagnostic))))
      (message text)))
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  )


(leaf flymake-codespell
  :doc "Flymake backend for codespell"
  :req "emacs-26.1" "compat-29.1.4.2"
  :tag "extensions" "emacs>=26.1"
  :url "https://www.github.com/skangas/flymake-codespell"
  :added "2025-11-24"
  :emacs>= 26.1
  :ensure t
  :when (executable-find "codespell")
  :after compat
  :preface
  (when (not (executable-find "codespell"))
    (message "**** Configure flymake-codespell 'codespell' not found"))
  :hook
  ((prog-mode-hook text-mode-hook) . flymake-codespell-setup-backend))

;; Put credentials in ~/.authinfo.gpg enctyped file and set them for packages needing below snippet
;; (require 'auth-source)
;; (let* ((auth (car (auth-source-search
;; 	       :host "jira.cc.bmwgroup.net"
;; 	       :requires '(:user :secret))))
;;    (password (funcall (plist-get auth :secret)))
;;    (username (plist-get auth :user)))
;;   (message "creds %S USR: %S AUTH: %S" `(:username ,(plist-get auth :user) :password ,password) username auth))

(leaf python
  :doc "Python's flying circus support for Emacs"
  :tag "builtin"
  :added "2025-09-16"
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
    (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

  :hook
  ((python-mode-hook . eglot-ensure)))

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
  :hook (c-mode-hook . kb/c-bind-clang-format)
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
  :bind-keymap (org-mode-map ("C-c C-0" . verb-command-map))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((verb . t))))

(leaf highlight-doxygen
  :doc "Highlight Doxygen comments"
  :tag "faces"
  :url "https://github.com/Lindydancer/highlight-doxygen"
  :added "2025-09-05"
  :ensure t
  :hook (c-mode-common-hook c-ts-base-mode-hook))

(leaf ttl-mode
  :doc "Mode for Turtle (and Notation 3)"
  :url "https://github.com/nxg/ttl-mode"
  :added "2025-11-27"
  :ensure t
  :hook turn-on-font-lock
  :mode ("\\.n3" "\\.ttl"))

(leaf doxymin
  :doc "Create doxygen style docs the easy way"
  :req "emacs-28.1"
  :tag "docs)" "convenience" "(abbref" "emacs>=28.1"
  :url "https://gitlab.com/L0ren2/doxymin"
  :added "2025-11-24"
  :emacs>= 28.1
  :ensure t)

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

(leaf helpful
  :doc "A better *help* buffer"
  :req "emacs-25" "dash-2.18.0" "s-1.11.0" "f-0.20.0" "elisp-refs-1.2"
  :tag "lisp" "help" "emacs>=25"
  :url "https://github.com/Wilfred/helpful"
  :added "2025-09-16"
  :emacs>= 25
  :ensure t
  :bind
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-command] . #'helpful-command)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-key] . #'helpful-key)
   (emacs-lisp-mode-map
    ;; for this in lisp modes.
    ("C-c C-d" . #'helpful-at-point)
    ("C-h F" . #'helpful-function))))

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :added "2025-09-17"
  ;; :custom (eldoc-echo-area-use-multiline-p . nil)
  :config
  (eldoc-add-command
   'mouse-set-point
   'previous-line 'next-line
   'jcs-py-indent-up 'jcs-py-indent-down
   'left-char 'right-char
   'jcs-backward-word-capital 'jcs-forward-word-capital
   'beginning-of-line 'end-of-line))

(leaf eldoc-box
  :disabled t
  :doc "Display documentation in childframe"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/casouri/eldoc-box"
  :added "2025-10-06"
  :emacs>= 27.1
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode)
  :config
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(leaf json-snatcher
  :doc "Grabs the path to JSON values in a JSON file"
  :req "emacs-24"
  :tag "emacs>=24"
  :url "http://github.com/sterlingg/json-snatcher"
  :added "2025-09-17"
  :emacs>= 24
  :ensure t
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin."
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  :hook
  ((js-mode-hook js2-mode-hook js-ts-mode-hook) . js-mode-bindings))

(leaf json-mode
  :doc "Major mode for editing JSON files"
  :req "json-snatcher-1.0.0" "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/joshwnj/json-mode"
  :added "2025-09-17"
  :emacs>= 24.4
  :ensure t
  :after json-snatcher)

(leaf yaml-ts-mode
  :doc "tree-sitter support for YAML"
  :tag "builtin"
  :added "2025-09-22"
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook (yaml-ts-mode-hook . eglot-ensure)
  :custom-face (font-lock-variable-name-face . '((t (:foreground "#cba6f7"))))
  )

(leaf cmake-ts-mode
  :doc "tree-sitter support for CMake"
  :tag "builtin"
  :added "2025-09-23"
  :preface
  (defun kb/cmake-mode-setup ()
      (message "***** cmake-ts-mode custom")
      (setq-local fill-column 80)
      (indent-tabs-mode -1)
      ;; (auto-fill-mode)
      ;; (setq cmake-tab-width 4)
      ;; (setq indent-tabs-mode nil)
      ;; NOTE: default eglot-server-programs contains following setting for cmake-mode and cmake-ts-mode.
      ;; ,(eglot-alternatives '((("neocmakelsp" "--stdio") "cmake-language-server")))
      ;; This is causing problems as ("neocmakelsp" "--stdio") is improperly passed to (eglot--find-executable)
      ;; To override this set it explicit
      (add-to-list 'eglot-server-programs
                   '(cmake-mode . ("neocmakelsp" "--stdio")))
      (eglot-ensure))
  (defun kb/whitespace-progmode-setup ()
    "Enable whitespace mode for programming modes."
    (setq-local whitespace-style
                '(face
                  trailing
                  lines-tail
                  newline
                  empty
                  indentation
                  space-after-tab::tab
                  ;; space-after-tab
                  ;; big-indent
                  space-before-tab::tab
                  ;; space-before-tab
                  tab-mark
                  newline-mark
                  ))
    )
  :hook
  (cmake-ts-mode-hook . kb/cmake-mode-setup)
  (cmake-ts-mode-hook . kb/whitespace-progmode-setup)
)

(leaf cmake-font-lock
  :doc "Advanced, type aware, highlight support for CMake"
  :req "cmake-mode-0.0"
  :tag "languages" "faces"
  :url "https://github.com/Lindydancer/cmake-font-lock"
  :added "2025-09-23"
  :ensure t
  :config
  (add-to-list 'cmake-font-lock-modes 'cmake-ts-mode))

(leaf bazel
  :doc "Bazel support for Emacs."
  :req "emacs-28.1"
  :tag "languages" "build tools" "emacs>=28.1"
  :url "https://github.com/bazelbuild/emacs-bazel-mode"
  :added "2025-11-12"
  :emacs>= 28.1
  :ensure t)

(leaf nxml-mode
  :doc "a new XML mode"
  :tag "builtin" "xml" "languages" "hypermedia" "text"
  :added "2025-10-03"
  :preface
  (defun kb/nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn')t error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))
  :config
  (defun xml-find-file-hook ()
    (when (derived-mode-p 'nxml-mode)
      (which-function-mode t)
      (setq which-func-mode t)
      (add-hook 'which-func-functions 'kb/nxml-where t t)))

  (add-hook 'find-file-hook 'xml-find-file-hook t))

(leaf sparql-mode
  :doc "Edit and interactively evaluate SPARQL queries"
  :req "cl-lib-0.5" "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/ljos/sparql-mode"
  :added "2025-12-10"
  :defun (url-http-parse-response . url-http)
  :emacs>= 24.3
  :ensure t
  :mode ("\\.sparql$" "\\.rq$"))


;;; AI Tools
(leaf gptel
  :doc "Interact with ChatGPT or other LLMs"
  :req "emacs-27.1" "transient-0.7.4" "compat-30.1.0.0"
  :tag "tools" "convenience" "emacs>=27.1"
  :url "https://github.com/karthink/gptel"
  :added "2025-10-09"
  :emacs>= 27.1
  :ensure t
  :after compat
  :custom
  ;; OPTIONAL configuration
  (gptel-model . 'claude-3.7-sonnet)
  (gptel-backend . `,(gptel-make-gh-copilot "Copilot"))
  (gptel-expert-commands . t))


;;; Tools
(leaf vterm
  :doc "Fully-featured terminal emulator"
  :req "emacs-25.1"
  :tag "terminals" "emacs>=25.1"
  :url "https://github.com/akermu/emacs-libvterm"
  :added "2025-09-23"
  :emacs>= 25.1
  :ensure t)

(leaf emamux
  :doc "Interact with tmux"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/syohex/emacs-emamux"
  :added "2025-10-10"
  :emacs>= 24.3
  :ensure t
  :custom
  ;; for tmux version 2+ following settings need to be applied
  (emamux:get-buffers-regexp . "^\\(buffer[0-9]+\\): +\\([0-9]+\\) +\\(bytes\\): +[\"]\\(.*\\)[\"]")
  (emamux:show-buffers-with-index . t))


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

(leaf markdown-ts-mode
  :doc "Major mode for Markdown using Treesitter"
  :req "emacs-29.1"
  :tag "faces" "matching" "languages" "emacs>=29.1"
  :url "https://github.com/LionyxML/markdown-ts-mode"
  :added "2025-10-06"
  :emacs>= 29.1
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(leaf markdown-mode
  :disabled t
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-28.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=28.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2025-11-18"
  :emacs>= 28.1
  :ensure t)

(leaf doxymacs
  :doc "Emacs integration with Doxygen."
  :req "emacs-24.4" "compat-28.1"
  :tag "tools" "convenience" "c" "emacs>=24.4"
  :url "https://pniedzielski.github.io/doxymacs/"
  :added "2025-10-06"
  :emacs>= 24.4
  :ensure t
  :after compat
  :hook ((c-ts-mode-hook c++-ts-mode-hook) . doxymacs-mode)
  :bind
  (:c-ts-base-mode-map
   ;; Lookup documentation for the symbol at point.
   ("C-c d ?" . doxymacs-lookup)
   ;; Rescan your Doxygen tags file.
   ("C-c d r" . doxymacs-rescan-tags)
   ;; Prompt you for a Doxygen command to enter, and its
   ;; arguments.
   ("C-c d RET" . doxymacs-insert-command)
   ;; Insert a Doxygen comment for the next function.
   ("C-c d f" . doxymacs-insert-function-comment)
   ;; Insert a Doxygen comment for the current file.
   ("C-c d i" . doxymacs-insert-file-comment)
   ;; Insert a Doxygen comment for the current member.
   ("C-c d ;" . doxymacs-insert-member-comment)
   ;; Insert a blank multi-line Doxygen comment.
   ("C-c d m" . doxymacs-insert-blank-multiline-comment)
   ;; Insert a blank single-line Doxygen comment.
   ("C-c d s" . doxymacs-insert-blank-singleline-comment)
   ;; Insert a grouping comments around the current region.
   ("C-c d @" . doxymacs-insert-grouping-comments)
   ))

(leaf gnuplot
  :doc "Major-mode and interactive frontend for gnuplot"
  :req "emacs-28.1" "compat-30"
  :tag "plotting" "gnuplot" "data" "emacs>=28.1"
  :url "https://github.com/emacs-gnuplot/gnuplot"
  :added "2026-01-02"
  :emacs>= 28.1
  :ensure t
  :after compat
  ;; :config
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
)

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

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :added "2025-11-24"
  :ensure t
  :global-minor-mode t)


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

(leaf separedit
  :doc "Edit comment/string/docstring/code block in separate buffer"
  :req "emacs-25.1" "dash-2.18" "edit-indirect-0.1.11"
  :tag "docs" "languages" "tools" "emacs>=25.1"
  :url "https://github.com/twlz0ne/separedit.el"
  :added "2025-10-06"
  :emacs>= 25.1
  :ensure t
  :after edit-indirect
  :custom
  ;; (separedit-default-mode . 'markdown-mode)
  (separedit-preserve-string-indentation . t)
  (separedit-continue-fill-column . t)
  (separedit-write-file-when-execute-save . t)
  (separedit-remove-trailing-spaces-in-comment . t)
  :init
  (defun kb/bind-separedit () (keymap-set prog-mode-map "C-c '" #'separedit))
  :hook (prog-mode-hook . kb/bind-separedit)
  :bind
  ((:prog-mode-map
    ((kbd "C-c '") . #'separedit))
   (:minibuffer-local-map
    ((kbd "C-c '") . #'separedit))
   (:help-mode-map
    ((kbd "C-c '") . #'separedit))
   ;; (:helpful-mode-map
   ;;  ((kbd "C-c '") . #'separedit))
   )
  ;; :init
  ;; (require 'separedit)
  ;; (define-key prog-mode-map "C-c '" 'separedit)
  ;; (define-key c-mode-map "C-c '" 'separedit)
  ;; (define-key c++-mode-map "C-c '" 'separedit)
  )

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


;; Visuals

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library"
  :req "emacs-24.3"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :added "2025-09-16"
  :emacs>= 24.3
  ;; :ensure t
  ;; :after nerd-icons
  ;; :config
  ;; (setq nerd-icons-fonts-subdirectory (expand-file-name (concat user-emacs-directory "fonts/")))
  ;; (unless (file-exists-p (expand-file-name (concat nerd-icons-fonts-subdirectory (car nerd-icons-font-names)))) (nerd-icons-install-fonts t))
  )

(leaf nerd-icons-corfu
  :doc "Icons for Corfu via nerd-icons"
  :req "emacs-27.1" "nerd-icons-0.1.0"
  :tag "icons" "files" "convenience" "emacs>=27.1"
  :url "https://github.com/LuigiPiucco/nerd-icons-corfu"
  :added "2025-09-16"
  :emacs>= 27.1
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf nerd-icons-completion
  :doc "Add icons to completion candidates"
  :req "emacs-25.1" "nerd-icons-0.0.1" "compat-30"
  :tag "lisp" "emacs>=25.1"
  :url "https://github.com/rainstormstudio/nerd-icons-completion"
  :added "2025-07-01"
  :emacs>= 25.1
  :ensure t
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  :global-minor-mode t)

(leaf sideline
  :doc "Show information on the side"
  :req "emacs-28.1" "ht-2.4"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline"
  :added "2025-09-17"
  :emacs>= 28.1
  :ensure t
  :blackout t
  :hook ((flycheck-mode-hook . sideline-mode)
         (eglot-managed-mode-hook . sideline-mode)))

(leaf sideline-blame
  :doc "Show blame messages with sideline."
  :req "emacs-28.1" "sideline-0.1.0" "vc-msg-1.1.1"
  :tag "blame" "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline-blame"
  :added "2025-11-24"
  :emacs>= 28.1
  :ensure t
  :after sideline
  :config
  (add-to-list 'sideline-backends-right '(sideline-blame . down))
)

(leaf sideline-eglot
  :doc "Show eglot information with sideline."
  :req "emacs-29.1" "eglot-1.12.29" "sideline-0.1.0" "ht-2.4"
  :tag "eglot" "convenience" "emacs>=29.1"
  :url "https://github.com/emacs-sideline/sideline-eglot"
  :added "2026-01-07"
  :emacs>= 29.1
  :ensure t
  :after eglot sideline
  :config
  ;; (setq sideline-backends-left nil)
  (add-to-list 'sideline-backends-left '(sideline-eglot . up))
  )

(leaf sideline-flymake
  :doc "Show flymake errors with sideline"
  :req "emacs-28.1" "sideline-0.1.0"
  :tag "flymake" "convenience" "emacs>=28.1"
  :url "https://github.com/emacs-sideline/sideline-flymake"
  :added "2026-01-07"
  :emacs>= 28.1
  :ensure t
  :after sideline
  :config
  (setq sideline-flymake-show-backend-name t)
  (add-to-list 'sideline-backends-left '(sideline-flymake . down))
)

(leaf visible-mark
  :doc "Make marks visible"
  :req "emacs-28.1"
  :tag "faces" "color" "marking" "emacs>=28.1"
  :url "https://codeberg.org/ideasman42/emacs-visible-mark"
  :added "2025-11-26"
  :emacs>= 28.1
  :ensure t
  :init
  (defface visible-mark-active ;; put this before (require 'visible-mark)
    '((((type tty) (class mono)))
      (t (:background "magenta"))) "")
  :custom
  (visible-mark-max . 2)
  (visible-mark-faces . `(visible-mark-face1 visible-mark-face2))
  :global-minor-mode t)

(leaf page-break-lines
  :doc "Display ^L page breaks as tidy horizontal lines"
  :req "emacs-25.1"
  :tag "faces" "convenience" "emacs>=25.1"
  :url "https://github.com/purcell/page-break-lines"
  :added "2025-10-06"
  :emacs>= 25.1
  :ensure t
  :blackout t
  :global-minor-mode global-page-break-lines-mode)

(message "** Init finished")
;;; init.el ends here
