#!/usr/bin/env bash
# deploy_fonts_no_bazel.sh — Download and install programming fonts without Bazel.
#
# Downloads the same fonts as the Bazel-managed //:fonts target:
#   - JetBrains Mono 2.304
#   - Fira Code 6.2
#   - Noto Sans Mono (Regular + Bold)
#
# Fonts are installed to ~/.local/share/fonts/emacs/ and registered via fc-cache.
#
# Usage:
#   ./deploy_fonts_no_bazel.sh              # install all fonts
#   ./deploy_fonts_no_bazel.sh --remove     # remove the emacs font directory
#   ./deploy_fonts_no_bazel.sh --list       # list currently installed fonts

set -euo pipefail

FONT_DIR="${HOME}/.local/share/fonts/emacs"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "${TMP_DIR}"' EXIT

# ---------------------------------------------------------------------------
usage() {
    echo "Usage: $0 [--remove | --list]"
    echo ""
    echo "  (no args)  Download and install fonts to ${FONT_DIR}"
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
# Verify sha256 of a downloaded file.
verify_sha256() {
    local file="$1" expected="$2"
    local actual
    actual="$(sha256sum "${file}" | awk '{print $1}')"
    if [[ "${actual}" != "${expected}" ]]; then
        echo "ERROR: SHA256 mismatch for ${file}" >&2
        echo "  expected: ${expected}" >&2
        echo "  actual:   ${actual}" >&2
        exit 1
    fi
}

mkdir -p "${FONT_DIR}"

# ---------------------------------------------------------------------------
# JetBrains Mono 2.304
echo "==> Downloading JetBrains Mono 2.304 ..."
JBMONO_ZIP="${TMP_DIR}/JetBrainsMono-2.304.zip"
curl -fsSL -o "${JBMONO_ZIP}" \
    "https://github.com/JetBrains/JetBrainsMono/releases/download/v2.304/JetBrainsMono-2.304.zip"
verify_sha256 "${JBMONO_ZIP}" \
    "6f6376c6ed2960ea8a963cd7387ec9d76e3f629125bc33d1fdcd7eb7012f7bbf"
unzip -q "${JBMONO_ZIP}" "fonts/ttf/*.ttf" -d "${TMP_DIR}/jbmono"
cp "${TMP_DIR}/jbmono/fonts/ttf/"*.ttf "${FONT_DIR}/"
echo "   Installed JetBrains Mono."

# ---------------------------------------------------------------------------
# Fira Code 6.2
echo "==> Downloading Fira Code 6.2 ..."
FIRA_ZIP="${TMP_DIR}/Fira_Code_v6.2.zip"
curl -fsSL -o "${FIRA_ZIP}" \
    "https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip"
verify_sha256 "${FIRA_ZIP}" \
    "0949915ba8eb24d89fd93d10a7ff623f42830d7c5ffc3ecbf960e4ecad3e3e79"
unzip -q "${FIRA_ZIP}" "ttf/*.ttf" -d "${TMP_DIR}/firacode"
cp "${TMP_DIR}/firacode/ttf/"*.ttf "${FONT_DIR}/"
echo "   Installed Fira Code."

# ---------------------------------------------------------------------------
# Noto Sans Mono (Regular + Bold)
echo "==> Downloading Noto Sans Mono ..."
curl -fsSL -o "${FONT_DIR}/NotoSansMono-Regular.ttf" \
    "https://github.com/googlefonts/noto-fonts/raw/main/hinted/ttf/NotoSansMono/NotoSansMono-Regular.ttf"
verify_sha256 "${FONT_DIR}/NotoSansMono-Regular.ttf" \
    "d9e2b23d19f8230be7146f409a52b1d23117e635e28f2e2892cf91b7382f325b"

curl -fsSL -o "${FONT_DIR}/NotoSansMono-Bold.ttf" \
    "https://github.com/googlefonts/noto-fonts/raw/main/hinted/ttf/NotoSansMono/NotoSansMono-Bold.ttf"
verify_sha256 "${FONT_DIR}/NotoSansMono-Bold.ttf" \
    "beb857bd799a114de7d5d90cc75cc69f4b6ee8d401f4ab20697e6f9719a66dfc"
echo "   Installed Noto Sans Mono."

# ---------------------------------------------------------------------------
echo "==> Refreshing font cache ..."
fc-cache -f "${FONT_DIR}"

echo ""
echo "Done. Fonts installed:"
ls -1 "${FONT_DIR}" | sed 's/^/  /'
echo ""
echo "Verify with:"
echo "  fc-list | grep -iE 'JetBrains|FiraCode|Noto.*Mono'"
