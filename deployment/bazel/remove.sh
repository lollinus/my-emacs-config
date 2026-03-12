#!/usr/bin/env bash
# remove.sh — uninstall Emacs from ~/.local/
# Use this to cleanly remove an Emacs build deployed by deploy.sh.
set -euo pipefail

VERSION="${1:-30.2}"
PREFIX="${HOME}/.local"

echo "==> Removing Emacs ${VERSION} from ${PREFIX}"

# Binaries installed by Emacs
for bin in emacs "emacs-${VERSION}" emacsclient etags ctags ebrowse; do
    rm -f "${PREFIX}/bin/${bin}"
done

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
