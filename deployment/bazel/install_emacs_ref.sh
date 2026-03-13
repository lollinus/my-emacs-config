#!/usr/bin/env bash
# install_emacs_ref.sh — install a ref/commit Emacs build into ~/.local/
#
# Invoked by: EMACS_REF=emacs-30.2 bazel run //:install_ref
# The pre-built tarball is supplied as a Bazel runfile by the sh_binary target.
set -euo pipefail

RUNFILES="${RUNFILES_DIR:-${BASH_SOURCE[0]}.runfiles}"
TARBALL="${RUNFILES}/_main/emacs-ref-install.tar.xz"

if [[ ! -f "${TARBALL}" ]]; then
    echo "ERROR: tarball not found: ${TARBALL}" >&2
    echo "Run 'EMACS_REF=<ref> bazel build //:emacs_ref_package' to build it first." >&2
    exit 1
fi

echo "==> Installing Emacs (ref build) to ${HOME}/.local/ ..."
mkdir -p "${HOME}/.local"
tar -xJf "${TARBALL}" -C "${HOME}/.local/"
update-desktop-database "${HOME}/.local/share/applications" 2>/dev/null || true
echo "Done. $("${HOME}/.local/bin/emacs" --version 2>/dev/null | head -1)"
