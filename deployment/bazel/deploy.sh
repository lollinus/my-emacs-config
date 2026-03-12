#!/usr/bin/env bash
# deploy.sh — Build Emacs via Bazel and install it to ~/.local/
#
# Usage:
#   ./deploy.sh           # build Emacs 30.2 (stable) and deploy
#   ./deploy.sh --git     # build Emacs master and deploy
#
# This script is a convenience wrapper around:
#   bazel build //:emacs_package && <extract>
#   bazel build //:emacs_git_package && <extract>
#
# Alternatively, use the Bazel run targets directly:
#   bazel run //:deploy
#   bazel run //:deploy_git

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PREFIX="${HOME}/.local"

USE_GIT=0
for arg in "$@"; do
  case "$arg" in
    --git) USE_GIT=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 1 ;;
  esac
done

cd "${SCRIPT_DIR}"

if [[ "${USE_GIT}" -eq 1 ]]; then
  echo "==> Building Emacs git master (WARNING: bleeding-edge)"
  bazel build //:emacs_git_package
  TARBALL="$(bazel info bazel-bin)/emacs-git-install.tar.xz"
else
  echo "==> Building Emacs 30.2 (stable)"
  bazel build //:emacs_package
  TARBALL="$(bazel info bazel-bin)/emacs-30.2-install.tar.xz"
fi

echo "==> Deploying to ${PREFIX}"
mkdir -p "${PREFIX}"
tar -xJf "${TARBALL}" -C "${PREFIX}"

update-desktop-database "${PREFIX}/share/applications" 2>/dev/null || true

echo "==> Done: $("${PREFIX}/bin/emacs" --version | head -1)"
echo ""
echo "Add ~/.local/bin to PATH if not already present:"
echo "  export PATH=\"\${HOME}/.local/bin:\${PATH}\""
