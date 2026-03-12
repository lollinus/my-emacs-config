"""emacs_fetch.bzl — Module extension that fetches Emacs source archives.

Provides the emacs_sources extension used by MODULE.bazel:
  emacs_sources.stable(version, url, sha256, strip_prefix)
    Downloads and extracts the official GNU Emacs tarball.
    The result is available as @emacs_src.

For git-based builds (any branch/tag/commit), no archive is pre-fetched.
The BUILD.bazel genrule clones directly at build time via EMACS_REF env var.
"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

_stable_tag = tag_class(attrs = {
    "version": attr.string(mandatory = True),
    "url": attr.string(mandatory = True),
    "sha256": attr.string(mandatory = True),
    "strip_prefix": attr.string(mandatory = True),
})

def _emacs_sources_impl(module_ctx):
    for mod in module_ctx.modules:
        for tag in mod.tags.stable:
            # Tag attributes are accessed directly (not via .attr).
            http_archive(
                name = "emacs_src",
                urls = [tag.url],
                sha256 = tag.sha256,
                strip_prefix = tag.strip_prefix,
                build_file_content = """\
filegroup(
    name = "all",
    srcs = glob(["**"], exclude = [
        "**/.git/**",
        "**/BUILD",
        "**/BUILD.bazel",
    ]),
    visibility = ["//visibility:public"],
)
# Export configure as a named file so genrules can find the source root
# via $(location @emacs_src//:configure) without expanding all sources.
exports_files(["configure"])
""",
            )

emacs_sources = module_extension(
    implementation = _emacs_sources_impl,
    tag_classes = {"stable": _stable_tag},
)
