;;; rc-docker.el --- Docker build helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Functions for constructing docker run commands used in compile-command

;;; Code:

(defgroup kb/docker nil
  "Docker build configuration for projects."
  :group 'kb-config)

;; These are declared buffer-local (via :safe) so .dir-locals.el can
;; set them per-project without an eval wrapper.

(defcustom kb/docker-image-repo ""
  "Docker image repository prefix, including trailing slash (e.g. \"myorg/\")."
  :type 'string
  :safe #'stringp
  :group 'kb/docker)

(defcustom kb/docker-image-name "app-build"
  "Docker image name.  Also used as the container --name."
  :type 'string
  :safe #'stringp
  :group 'kb/docker)

(defcustom kb/docker-image-version "latest"
  "Docker image version tag."
  :type 'string
  :safe #'stringp
  :group 'kb/docker)

(defcustom kb/docker-guest-home "~/.app-build-home/"
  "Host directory bound to the container home directory."
  :type 'string
  :safe #'stringp
  :group 'kb/docker)

(defcustom kb/docker-project-path "~/projects/myproject"
  "Host source directory; bind-mounted at the same path inside the container."
  :type 'string
  :safe #'stringp
  :group 'kb/docker)

;;;###autoload
(defun kb/docker-build-command (build-cmd &optional image guest-home project-path)
  "Return a docker run command string for a project build container.
BUILD-CMD is the command to execute inside the container.

Optional overrides (fall back to the corresponding defcustom when nil):
  IMAGE        Full image reference \"repo/name:version\".
  GUEST-HOME   Host path bound to the container home directory.
  PROJECT-PATH Host source path; bind-mounted at the same path in container.

When called interactively, prompts for BUILD-CMD and sets `compile-command'."
  (interactive
   (list (read-string "Build command: "
                      (and (boundp 'compile-command) compile-command))))
  (let* ((user    (format "%d:%d" (user-uid) (group-gid)))
         (g-home  (expand-file-name (or guest-home kb/docker-guest-home)))
         (home    (expand-file-name "~"))
         (src     (expand-file-name (or project-path kb/docker-project-path)))
         (img     (or image
                      (concat kb/docker-image-repo
                              kb/docker-image-name
                              ":" kb/docker-image-version)))
         (cmd (string-join
               (list "docker" "run" "--rm" "--init"
                     "--user"   user
                     "-v"       "/run/systemd/resolve/resolv.conf:/etc/resolv.conf:ro"
                     "-v"       (concat g-home ":" home)
                     "--memory-swap=-1"
                     "--ulimit" "core=-1"
                     (concat "--name=" kb/docker-image-name)
                     (concat "--workdir=" src)
                     "--mount"  (concat "type=bind,source=" src ",target=" src)
                     img
                     build-cmd)
               " ")))
    (when (called-interactively-p 'any)
      (setq compile-command cmd))
    cmd))

(provide 'rc-docker)
;;; rc-docker.el ends here
