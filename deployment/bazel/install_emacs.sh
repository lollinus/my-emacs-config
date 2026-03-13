#!/usr/bin/env bash
# install_emacs.sh — install the stable Emacs 30.2 build into ~/.local/
#
# Invoked by: bazel run //:install
# The pre-built tarball is supplied as a Bazel runfile by the sh_binary target.
set -euo pipefail

RUNFILES="${RUNFILES_DIR:-${BASH_SOURCE[0]}.runfiles}"
TARBALL="${RUNFILES}/_main/emacs-30.2-install.tar.xz"

if [[ ! -f "${TARBALL}" ]]; then
    echo "ERROR: tarball not found: ${TARBALL}" >&2
    echo "Run 'bazel build //:emacs_package' to build it first." >&2
    exit 1
fi

echo "==> Installing Emacs to ${HOME}/.local/ ..."
mkdir -p "${HOME}/.local"
tar -xJf "${TARBALL}" -C "${HOME}/.local/"
update-desktop-database "${HOME}/.local/share/applications" 2>/dev/null || true
echo "Done. $("${HOME}/.local/bin/emacs" --version 2>/dev/null | head -1)"
