# Copilot Instructions for .emacs.d

This repository is a personal Emacs configuration targeting Emacs ≥27.1 (many features require 29+).

## Architecture

### Package Management: `leaf` + `package.el`
All packages are declared using [leaf](https://github.com/conao3/leaf.el), a lightweight configuration macro. `leaf` is bootstrapped in `init.el` early on and replaces `use-package`. Package archives are `gnu`, `melpa`, and `org`.

### File Layout
- **`early-init.el`** — frame setup before GUI loads (bars, title bar, `custom-file`)
- **`init.el`** — main config (~2,400 lines); organized into: Startup → Basics → Development → AI Tools → Tools → local overrides
- **`init.rest.el`** — optional language configs (Haskell, OCaml, LaTeX) loaded on demand
- **`custom.el`** — `custom-set-variables` / `custom-set-faces`; `custom-file` points here
- **`local.el`** — machine-local overrides (gitignored; copy from `local.el.example`)
- **`rc/`** — helper elisp loaded via `load-path`:
  - `rc-functions.el` — core helpers (`kb/treesit-ensure`, `kb/mason-ensure`, editing utils)
  - `rc-alpha.el` — frame transparency toggle
  - `kb-secrets.el` / `my-secrets.el.gpg` — credential helpers

### Custom `leaf` Keywords
Two project-specific keywords extend `leaf`:

- **`:treesit LANG...`** — calls `kb/treesit-ensure` to auto-install tree-sitter grammars at config time
- **`:mason (EXEC PKG [MSG])...`** — calls `kb/mason-ensure` to install LSP servers/formatters via mason.el

These are defined in `rc/rc-functions.el` and added via `leaf-keywords-init`.

### Technology Stack
| Concern | Package(s) |
|---|---|
| Completion UI | vertico, consult, corfu, cape, embark, marginalia |
| LSP | eglot (built-in ≥29) |
| Diagnostics | flymake, sideline-eglot, sideline-flymake |
| Syntax / Folding | tree-sitter (built-in ≥29), treesit-fold |
| Git | magit, diff-hl, git-timemachine, sideline-blame |
| AI | copilot, gptel, ai-code |
| Terminal | vterm |
| Icons | nerd-icons |
| Theming | circadian (time-based), zerodark |

## Key Conventions

### Naming
- All custom functions and variables are prefixed `kb/` (author initials).

### Adding a Package
Follow the `leaf` pattern used throughout `init.el`:
```elisp
(leaf package-name
  :ensure t
  :after some-dependency        ; optional ordering
  :hook (some-mode-hook . package-mode)
  :bind (("C-c x" . package-command))
  :treesit python               ; auto-install grammar
  :mason ("pyright" "pyright") ; auto-install LSP server
  :config
  (setq package-option t))
```

### Tree-sitter Modes
Modern tree-sitter variants (`c-ts-mode`, `python-ts-mode`, `typescript-ts-mode`, etc.) are preferred over legacy modes. Grammars are fetched automatically via `:treesit`.

### Machine-local Config
Never commit `local.el`. Machine-specific overrides (Copilot enterprise URL, GitHub Enterprise auth, etc.) go in `local.el` based on `local.el.example`.

### Secrets
Credentials are managed via `kb-secrets.el` and the GPG-encrypted `my-secrets.el.gpg`. Do not add plaintext secrets to any committed file.

## Build / Setup

The simplest path is `sudo snap install emacs --classic` (see `deployment/snap-ubuntu.md`).

To build from source on Ubuntu 24.04, use `deployment/emacs-config-ubuntu24.sh` (Emacs 30.2,
PGTK/Wayland, tree-sitter, native compilation AOT, installs to `~/.local/`).
A Bazel-based reproducible build is available in `deployment/bazel/`.
For RHEL-family systems (AlmaLinux 9, Rocky Linux 9, CentOS Stream 9), see `deployment/build.emacs.org`.

System package prerequisites for Ubuntu 24.04 are in `deployment/emacs-pkgs-ubuntu24.txt`.

After launching Emacs with this config for the first time, packages auto-install from MELPA/GNU ELPA. There is no separate install step.
