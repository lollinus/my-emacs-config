#!/usr/bin/env bash
# remove_fonts.sh — uninstall fonts from ~/.local/share/fonts/emacs/
#
# Invoked by: bazel run //:remove_fonts
set -euo pipefail

FONT_DIR="${HOME}/.local/share/fonts/emacs"

if [[ ! -d "${FONT_DIR}" ]]; then
    echo "No fonts found at ${FONT_DIR} — nothing to remove."
    exit 0
fi

rm -rf "${FONT_DIR}"
fc-cache -f "${HOME}/.local/share/fonts" 2>/dev/null || true
echo "Removed ${FONT_DIR} and refreshed font cache."
