#!/usr/bin/env bash
# remove.sh — uninstall Emacs from ~/.local/
# Use this to cleanly remove an Emacs build deployed by deploy.sh.
set -euo pipefail

VERSION="${1:-30.2}"
PREFIX="${HOME}/.local"

echo "==> Removing Emacs ${VERSION} from ${PREFIX}"

# Versioned binary (e.g. emacs-30.2, emacs-31.0.50)
rm -f "${PREFIX}/bin/emacs-${VERSION}"

# Remove the 'emacs' symlink only if it points at this version's binary.
# Skipping it when it points elsewhere prevents accidentally breaking a
# different installed version (e.g. removing 30.2 while 31 is active).
EMACS_LINK="${PREFIX}/bin/emacs"
if [[ -L "${EMACS_LINK}" && "$(readlink "${EMACS_LINK}")" == "emacs-${VERSION}" ]]; then
    rm -f "${EMACS_LINK}"
else
    echo "    Skipping ${EMACS_LINK} — points to $(readlink "${EMACS_LINK}" 2>/dev/null || echo '(not a symlink)'), not emacs-${VERSION}"
fi

# Remove helper binaries only when emacs-VERSION was actually the active build
# (i.e. the emacs symlink pointed at it, meaning we just removed it above).
# If a different version is active, leave the helpers in place.
if [[ ! -L "${EMACS_LINK}" && ! -f "${EMACS_LINK}" ]]; then
    for bin in emacsclient etags ctags ebrowse; do
        rm -f "${PREFIX}/bin/${bin}"
    done
fi

# Versioned data directories
rm -rf "${PREFIX}/share/emacs/${VERSION}"
rm -rf "${PREFIX}/lib/emacs/${VERSION}"
rm -rf "${PREFIX}/libexec/emacs/${VERSION}"

# GLib schema registered by Emacs
SCHEMA="${PREFIX}/share/glib-2.0/schemas/org.gnu.emacs.defaults.gschema.xml"
if [[ -f "${SCHEMA}" ]]; then
    rm -f "${SCHEMA}"
    glib-compile-schemas "${PREFIX}/share/glib-2.0/schemas/" 2>/dev/null || true
fi

# site-lisp only if it contains nothing user-added (just the generated subdirs.el)
SITE_LISP="${PREFIX}/share/emacs/site-lisp"
if [[ -d "${SITE_LISP}" ]]; then
    EXTRA=$(find "${SITE_LISP}" -not -name subdirs.el -not -type d | wc -l)
    if [[ "${EXTRA}" -eq 0 ]]; then
        rm -rf "${SITE_LISP}"
    else
        echo "    Skipping ${SITE_LISP} — contains user files"
    fi
fi

echo "==> Done. Emacs ${VERSION} removed from ${PREFIX}"
