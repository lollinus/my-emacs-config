"""emacs_fetch.bzl — Module extension that fetches Emacs source archives.

Provides two repository rules used by MODULE.bazel:
  - emacs_sources.stable(version, url, sha256, strip_prefix)
      Downloads and extracts the official GNU Emacs tarball.
      The result is available as @emacs_src.

  - For git master builds no archive is pre-fetched; instead the BUILD.bazel
    genrule clones directly at build time (see :emacs_git_package target).
    This means git builds are never cached by Bazel's content-addressable store.
"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# ---------------------------------------------------------------------------
# Repository rule for a versioned tarball
# ---------------------------------------------------------------------------

def _stable_impl(ctx):
    """Download and expose the Emacs stable source tarball as @emacs_src."""
    http_archive(
        name = "emacs_src",
        urls = [ctx.attr.url],
        sha256 = ctx.attr.sha256,
        strip_prefix = ctx.attr.strip_prefix,
        # Provide a minimal BUILD file so Bazel can reference all source files.
        build_file_content = """
filegroup(
    name = "all",
    srcs = glob(["**"], exclude = [
        "**/.git/**",
        "**/BUILD",
        "**/BUILD.bazel",
    ]),
    visibility = ["//visibility:public"],
)
""",
    )

_stable_attrs = {
    "version": attr.string(mandatory = True),
    "url": attr.string(mandatory = True),
    "sha256": attr.string(mandatory = True),
    "strip_prefix": attr.string(mandatory = True),
}

_stable_tag = tag_class(attrs = _stable_attrs)

# ---------------------------------------------------------------------------
# Module extension entry point
# ---------------------------------------------------------------------------

def _emacs_sources_impl(module_ctx):
    for mod in module_ctx.modules:
        for tag in mod.tags.stable:
            _stable_impl(tag)

emacs_sources = module_extension(
    implementation = _emacs_sources_impl,
    tag_classes = {"stable": _stable_tag},
)
