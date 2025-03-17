;;; package --- kb-secrets
;;; Commentary:
;;; put secrets here and never commit
;;; (secrets-create-item "default" "SOME SECRET ID" "SECRET" :user "USER" :host "HOST")

(require 'secrets)


(secrets-set-alias "login" "default")

;;=====================================================================
;; NO COMMIT BEGIN
;; NO COMMIT END
;;=====================================================================

(provide 'kb-secrets)
;;; kb-secrets.el ends here
