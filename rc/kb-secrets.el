;;; package --- kb-secrets  -*- lexical-binding: t; -*-
;;; Commentary:
;;; put secrets here and never commit
;;; (secrets-create-item "default" "SOME SECRET ID" "SECRET" :user "USER" :host "HOST")

(require 'secrets)
;;; Code:

(secrets-set-alias "login" "default")

(defun kb-remove-secret-by-name (name)
  "Remove secrets represented by name ITEM from secrets."
  (interactive)
  (mapcar (lambda (item)
            (message "processing: %s" item)
             (secrets-delete-item "default" item))
         (seq-filter (lambda (x)
                       (message "checking: %s" x)
                       (equal x name))
                     (secrets-list-items "default"))))

(defvar kb-secret-collection "default")

;; (defconst SECRET_NAME "MY_PRECIOUS")
;; (kb-remove-secret-by-name "SECRET_NAME")
;; (defvar kb-secret-path (secrets-item-path kb-secret-collection SECRET_NAME))
;; (secrets-get-secret kb-secret-collection kb-secret-path)
;; (secrets-delete-item kb-secret-collection kb-secret-path)


(defun kb-secrets-update-secret (collection item-path secret &rest attributes)
  "Compare SECRET with stored in COLLECTION represented by ITEM-PATH.

When doesn't match delete secret and recreate with new secret.

TODO: compare and update attributes."
  (unless (string-equal (secrets-get-secret collection item-path) secret)
    (message "Update secret: %s:%s" collection item-path)
    (secrets-delete-item collection item-path)
    (apply #'secrets-create-item collection item-path secret attributes)))

(defun kb-secrets-create-or-update-secret (item secret &rest attributes)
  "Check whether secret ITEM already exits. When not found create it. When found compare SECRET and update if differ."
  (let* ((kb-collection kb-secret-collection)
         (item-path (secrets-item-path kb-collection item))
         (arg-list (append '("default" item secret) attributes)))
    (if (secrets-empty-path item-path)
        ;; not found so create new item
          (apply #'secrets-create-item kb-collection item secret attributes)
      ;; Update existing
      (apply #'kb-secrets-update-secret kb-collection item-path secret attributes))))

;;=====================================================================
;; Actual secrets are encrypted using GPG

(defvar kb-secret-list ())
;; NO COMMIT BEGIN
;; Put secrets there in form:
;; ("SECRET_NAME" "password" :user "USER" :host "optional host" :email "optional email" :xdg:schema "optional")
(load "my-secrets.el.gpg" 'noerror 'nomessage)
;; NO COMMIT END
;;=====================================================================

(mapcar (lambda (item)
          (let* ((item-name (car item))
                (secret (cadr item))
                (attributes (cddr item)))
            (apply #'kb-secrets-create-or-update-secret item-name secret attributes)))
        kb-secret-list)

(provide 'kb-secrets)
;;; kb-secrets.el ends here
