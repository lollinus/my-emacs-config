#!/usr/bin/env bash
# deploy.sh — Build Emacs via Bazel and install it to ~/.local/
#
# Usage:
#   ./deploy.sh                    # Emacs 30.2 stable (pinned, recommended)
#   ./deploy.sh --ref master       # git master
#   ./deploy.sh --ref emacs-30     # release branch
#   ./deploy.sh --ref emacs-30.2   # release tag
#   ./deploy.sh --ref a1b2c3d4     # specific commit SHA
#
# Bazel caches each unique EMACS_REF value separately.
# For mutable refs (master, emacs-30), force a fresh build with:
#   ./deploy.sh --ref master --force

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PREFIX="${HOME}/.local"

EMACS_REF=""
FORCE=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --ref)    EMACS_REF="${2:?--ref requires an argument}"; shift 2 ;;
    --ref=*)  EMACS_REF="${1#--ref=}"; shift ;;
    --force)  FORCE=1; shift ;;
    *) echo "Unknown argument: $1" >&2
       echo "Usage: $0 [--ref REF] [--force]" >&2; exit 1 ;;
  esac
done

cd "${SCRIPT_DIR}"

if [[ -n "${EMACS_REF}" ]]; then
  echo "==> Building Emacs ref '${EMACS_REF}'"
  FORCE_FLAG=""
  if [[ "${FORCE}" -eq 1 ]]; then
    FORCE_FLAG="--action_env=EMACS_REF_FORCE=$(date +%s)"
  fi
  # shellcheck disable=SC2086
  EMACS_REF="${EMACS_REF}" bazel --nohome_rc build //:emacs_ref_package ${FORCE_FLAG}
  TARBALL="$(bazel --nohome_rc info bazel-bin)/emacs-ref-install.tar.xz"
else
  echo "==> Building Emacs 30.2 (stable)"
  bazel --nohome_rc build //:emacs_package
  TARBALL="$(bazel --nohome_rc info bazel-bin)/emacs-30.2-install.tar.xz"
fi

echo "==> Deploying to ${PREFIX}"
mkdir -p "${PREFIX}"
tar -xJf "${TARBALL}" -C "${PREFIX}"

update-desktop-database "${PREFIX}/share/applications" 2>/dev/null || true

echo "==> Done: $("${PREFIX}/bin/emacs" --version | head -1)"
echo ""
echo "Add ~/.local/bin to PATH if not already present:"
echo "  export PATH=\"\${HOME}/.local/bin:\${PATH}\""
