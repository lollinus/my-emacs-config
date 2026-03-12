#!/usr/bin/env bash
# install_fonts.sh — install programming fonts into ~/.local/share/fonts/emacs/
#
# Invoked by: bazel run //:install_fonts
# The pre-built fonts.tar.gz is supplied as a Bazel runfile by the sh_binary target.
set -euo pipefail

RUNFILES="${RUNFILES_DIR:-${BASH_SOURCE[0]}.runfiles}"
TARBALL="${RUNFILES}/__main__/fonts.tar.gz"

if [[ ! -f "${TARBALL}" ]]; then
    echo "ERROR: tarball not found: ${TARBALL}" >&2
    echo "Run 'bazel build //:fonts' to build it first." >&2
    exit 1
fi

FONT_DIR="${HOME}/.local/share/fonts/emacs"
mkdir -p "${FONT_DIR}"
echo "==> Installing fonts to ${FONT_DIR} ..."
tar -xzf "${TARBALL}" -C "${FONT_DIR}"
fc-cache -f "${FONT_DIR}"
echo "Done. Fonts installed:"
ls -1 "${FONT_DIR}" | sed 's/^/  /'
echo ""
echo "Verify with: fc-list | grep -iE 'JetBrains|FiraCode|Noto.*Mono'"
