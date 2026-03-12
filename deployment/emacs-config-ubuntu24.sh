#!/usr/bin/env bash
# emacs-config-ubuntu24.sh — Build and install Emacs from source on Ubuntu 24.04 LTS
#
# Usage:
#   ./emacs-config-ubuntu24.sh           # build Emacs 30.2 (stable, recommended)
#   ./emacs-config-ubuntu24.sh --git     # build latest git master (bleeding-edge)
#
# The binary is installed to ~/.local/. No sudo required for the install step.
# Prerequisites (run once):
#   sudo apt install $(grep -v '^#' emacs-pkgs-ubuntu24.txt | tr '\n' ' ')

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

EMACS_VERSION="30.2"
EMACS_TARBALL_URL="https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz"
EMACS_GIT_URL="https://git.savannah.gnu.org/git/emacs.git"
EMACS_GIT_BRANCH="master"

INSTALL_PREFIX="${HOME}/.local"
BUILD_DIR="${HOME}/emacs-build"
JOBS="$(nproc --ignore=2)"

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------

USE_GIT=0
for arg in "$@"; do
  case "$arg" in
    --git) USE_GIT=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 1 ;;
  esac
done

# ---------------------------------------------------------------------------
# Configure flags shared between stable and git builds
# ---------------------------------------------------------------------------

CONFIGURE_FLAGS=(
  "--prefix=${INSTALL_PREFIX}"
  "--with-pgtk"                     # Pure GTK: supports Wayland and X11
  "--with-native-compilation=aot"   # Ahead-of-time native compilation
  "--with-tree-sitter"              # Built-in tree-sitter support
  "--with-json"                     # JSON via libjansson
  "--with-modules"                  # Dynamic modules
  "--with-sqlite3"                  # SQLite3 support
  "--with-harfbuzz"                 # HarfBuzz text shaping
  "--with-compress-install"         # Compress installed .el files
  "--with-threads"
  "--with-rsvg"                     # SVG image support
  "--with-jpeg"
  "--with-png"
  "--with-gif"
  "--with-tiff"
  "--with-xpm"
  "--with-gnutls"
  "--with-dbus"
  "--with-mailutils"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

step() { echo; echo "==> $*"; }

check_deps() {
  step "Checking build dependencies"
  local missing=()
  for pkg in libgccjit-13-dev libtree-sitter-dev libgtk-3-dev libharfbuzz-dev; do
    dpkg -s "$pkg" &>/dev/null || missing+=("$pkg")
  done
  if [[ ${#missing[@]} -gt 0 ]]; then
    echo "ERROR: Missing packages: ${missing[*]}"
    echo "Run: sudo apt install ${missing[*]}"
    exit 1
  fi
}

# ---------------------------------------------------------------------------
# Source acquisition
# ---------------------------------------------------------------------------

acquire_source() {
  mkdir -p "${BUILD_DIR}"

  if [[ "${USE_GIT}" -eq 1 ]]; then
    step "Cloning Emacs git master (bleeding-edge — may be unstable)"
    if [[ -d "${BUILD_DIR}/emacs/.git" ]]; then
      echo "Updating existing clone..."
      git -C "${BUILD_DIR}/emacs" fetch --depth=1 origin "${EMACS_GIT_BRANCH}"
      git -C "${BUILD_DIR}/emacs" reset --hard "origin/${EMACS_GIT_BRANCH}"
    else
      git clone --depth 1 --branch "${EMACS_GIT_BRANCH}" \
        "${EMACS_GIT_URL}" "${BUILD_DIR}/emacs"
    fi
    EMACS_SRC="${BUILD_DIR}/emacs"
  else
    step "Downloading Emacs ${EMACS_VERSION} source tarball"
    local tarball="${BUILD_DIR}/emacs-${EMACS_VERSION}.tar.xz"
    if [[ ! -f "${tarball}" ]]; then
      curl -fL --progress-bar -o "${tarball}" "${EMACS_TARBALL_URL}"
    else
      echo "Tarball already present, skipping download."
    fi
    step "Extracting tarball"
    tar -xJf "${tarball}" -C "${BUILD_DIR}"
    EMACS_SRC="${BUILD_DIR}/emacs-${EMACS_VERSION}"
  fi
}

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

build_emacs() {
  step "Running autogen.sh"
  cd "${EMACS_SRC}"
  # autogen.sh is needed for git checkouts; harmless for tarballs
  [[ -x ./autogen.sh ]] && ./autogen.sh

  step "Configuring (prefix=${INSTALL_PREFIX})"
  CC=clang ./configure "${CONFIGURE_FLAGS[@]}"

  step "Building with ${JOBS} parallel jobs"
  make -j"${JOBS}"
}

# ---------------------------------------------------------------------------
# Install
# ---------------------------------------------------------------------------

install_emacs() {
  step "Installing to ${INSTALL_PREFIX}"
  make install

  step "Updating desktop database"
  update-desktop-database "${INSTALL_PREFIX}/share/applications" 2>/dev/null || true

  step "Done"
  "${INSTALL_PREFIX}/bin/emacs" --version | head -1
  echo ""
  echo "Emacs installed to ${INSTALL_PREFIX}/bin/emacs"
  echo "Make sure ${INSTALL_PREFIX}/bin is on your PATH:"
  echo "  export PATH=\"\${HOME}/.local/bin:\${PATH}\""
}

# ---------------------------------------------------------------------------
# Package (optional — produces a redistributable tarball)
# ---------------------------------------------------------------------------

package_emacs() {
  local label
  if [[ "${USE_GIT}" -eq 1 ]]; then
    label="emacs-git-$(date +%Y%m%d)"
  else
    label="emacs-${EMACS_VERSION}"
  fi

  local out="${BUILD_DIR}/${label}-ubuntu24.tar.xz"
  step "Creating distributable tarball: ${out}"

  local tmp_install="${BUILD_DIR}/${label}-install"
  mkdir -p "${tmp_install}"
  make prefix="${tmp_install}" install

  tar -cJf "${out}" -C "${tmp_install}" .
  echo "Package: ${out}"

  # Generate matching uninstall script
  local uninstall="${BUILD_DIR}/${label}-uninstall.sh"
  {
    echo "#!/bin/sh"
    echo "# Uninstall ${label} from \${PREFIX:=${INSTALL_PREFIX}}"
    find "${tmp_install}" -type f -printf 'rm "${PREFIX}/%P"\n'
    find "${tmp_install}" -mindepth 1 -type d -printf 'rmdir "${PREFIX}/%P"\n' | sort -r
  } > "${uninstall}"
  chmod +x "${uninstall}"
  echo "Uninstall script: ${uninstall}"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

check_deps
acquire_source
build_emacs
install_emacs
package_emacs
