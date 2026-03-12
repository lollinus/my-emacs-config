#!/usr/bin/env bash
# deploy_fonts.sh — Download and install programming fonts to ~/.local/share/fonts/emacs/
#
# Uses Bazel to fetch font archives (JetBrains Mono, Fira Code, Noto Sans Mono)
# and installs them under ~/.local/share/fonts/emacs/ so they are available
# to fontconfig (and therefore to Emacs and all other GTK apps).
#
# Usage:
#   ./deploy_fonts.sh              # install all fonts
#   ./deploy_fonts.sh --remove     # remove the emacs font directory
#   ./deploy_fonts.sh --list       # list currently installed fonts

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FONT_DIR="${HOME}/.local/share/fonts/emacs"
BAZEL_TARGET="//:fonts"

# ---------------------------------------------------------------------------
usage() {
    echo "Usage: $0 [--remove | --list]"
    echo ""
    echo "  (no args)  Build //:fonts and install to ${FONT_DIR}"
    echo "  --remove   Remove ${FONT_DIR} and refresh font cache"
    echo "  --list     List fonts currently installed in ${FONT_DIR}"
    exit 1
}

remove_fonts() {
    if [[ ! -d "${FONT_DIR}" ]]; then
        echo "No fonts found at ${FONT_DIR} — nothing to remove."
        exit 0
    fi
    rm -rf "${FONT_DIR}"
    fc-cache -f "${HOME}/.local/share/fonts" 2>/dev/null || true
    echo "Removed ${FONT_DIR} and refreshed font cache."
}

list_fonts() {
    if [[ ! -d "${FONT_DIR}" ]]; then
        echo "No fonts installed at ${FONT_DIR}."
        exit 0
    fi
    echo "Fonts in ${FONT_DIR}:"
    ls -1 "${FONT_DIR}"
}

# ---------------------------------------------------------------------------
# Argument parsing
case "${1:-}" in
    --remove) remove_fonts; exit 0 ;;
    --list)   list_fonts;   exit 0 ;;
    "")       ;;
    *)        usage ;;
esac

# ---------------------------------------------------------------------------
# Build

cd "${SCRIPT_DIR}"
echo "==> Building ${BAZEL_TARGET} ..."
bazel --nohome_rc build "${BAZEL_TARGET}"

TARBALL="${SCRIPT_DIR}/bazel-bin/fonts.tar.gz"
if [[ ! -f "${TARBALL}" ]]; then
    echo "ERROR: Expected output not found: ${TARBALL}" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Install

mkdir -p "${FONT_DIR}"
echo "==> Installing fonts to ${FONT_DIR} ..."
tar -xzf "${TARBALL}" -C "${FONT_DIR}"

echo "==> Refreshing font cache ..."
fc-cache -f "${FONT_DIR}"

echo ""
echo "Done. Fonts installed:"
ls -1 "${FONT_DIR}" | sed 's/^/  /'
echo ""
echo "Verify with:"
echo "  fc-list | grep -iE 'JetBrains|FiraCode|Noto.*Mono'"
