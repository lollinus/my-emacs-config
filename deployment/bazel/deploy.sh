#!/usr/bin/env bash
# deploy.sh — Build Emacs via Bazel and install it to ~/.local/
#
# Usage:
#   ./deploy.sh                    # Emacs 30.2 stable, clang (default)
#   ./deploy.sh --cc gcc           # build with GCC instead of clang
#   ./deploy.sh --ref master       # git master
#   ./deploy.sh --ref emacs-30     # release branch
#   ./deploy.sh --ref emacs-30.2   # release tag
#   ./deploy.sh --ref a1b2c3d4     # specific commit SHA
#   ./deploy.sh --ref master --cc gcc --force
#   ./deploy.sh --ref master --reforce   # rebuild using last --force timestamp
#                                        # (config changed, sources unchanged)
#
# Bazel caches each unique (EMACS_REF, EMACS_CC) combination separately.
# For mutable refs (master, emacs-30), force a fresh build with --force.
# Use --reforce to rebuild (e.g. after Bazel config changes) without
# re-fetching sources: reuses the EMACS_REF_FORCE value from the last
# --force run, so Bazel treats source inputs as unchanged.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PREFIX="${HOME}/.local"
LAST_FORCE_FILE="${SCRIPT_DIR}/.last_force"
BAZELRC_FORCE_FILE="${SCRIPT_DIR}/.bazelrc.force"

EMACS_REF=""
EMACS_CC="${EMACS_CC:-clang}"
FORCE=0
REFORCE=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --ref)     EMACS_REF="${2:?--ref requires an argument}"; shift 2 ;;
    --ref=*)   EMACS_REF="${1#--ref=}"; shift ;;
    --cc)      EMACS_CC="${2:?--cc requires an argument}"; shift 2 ;;
    --cc=*)    EMACS_CC="${1#--cc=}"; shift ;;
    --force)   FORCE=1; shift ;;
    --reforce) REFORCE=1; shift ;;
    *) echo "Unknown argument: $1" >&2
       echo "Usage: $0 [--ref REF] [--cc clang|gcc] [--force|--reforce]" >&2; exit 1 ;;
  esac
done

cd "${SCRIPT_DIR}"

if [[ "${FORCE}" -eq 1 ]]; then
  FORCE_VALUE="$(date +%s)"
  echo "${FORCE_VALUE}" > "${LAST_FORCE_FILE}"
  echo "build --action_env=EMACS_REF_FORCE=${FORCE_VALUE}" > "${BAZELRC_FORCE_FILE}"
  echo "==> Saved EMACS_REF_FORCE=${FORCE_VALUE} to ${BAZELRC_FORCE_FILE}"
elif [[ "${REFORCE}" -eq 1 ]]; then
  if [[ ! -f "${LAST_FORCE_FILE}" ]]; then
    echo "ERROR: no saved force value found (${LAST_FORCE_FILE})." >&2
    echo "Run with --force first to record an EMACS_REF_FORCE value." >&2
    exit 1
  fi
  FORCE_VALUE="$(<"${LAST_FORCE_FILE}")"
  echo "build --action_env=EMACS_REF_FORCE=${FORCE_VALUE}" > "${BAZELRC_FORCE_FILE}"
  echo "==> Reusing EMACS_REF_FORCE=${FORCE_VALUE} from ${LAST_FORCE_FILE}"
fi

if [[ -n "${EMACS_REF}" ]]; then
  echo "==> Building Emacs ref '${EMACS_REF}' with ${EMACS_CC}"
  EMACS_REF="${EMACS_REF}" EMACS_CC="${EMACS_CC}" bazel --nohome_rc build //:emacs_ref_package
  TARBALL="$(bazel --nohome_rc info bazel-bin)/emacs-ref-install.tar.xz"
else
  echo "==> Building Emacs 30.2 (stable) with ${EMACS_CC}"
  EMACS_CC="${EMACS_CC}" bazel --nohome_rc build //:emacs_package
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
