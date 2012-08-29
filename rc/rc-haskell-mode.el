;;; rc-haskell-mode.el ---
;;(add-to-list 'load-path (concat my-site-lisp-directory "haskell-mode"))
(load (concat my-site-lisp-directory "haskell-mode/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; indentation modules are mutually exclusive. Only one of them can be used.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq haskell-program-name
      (if (eq system-type 'cygwin)
          "/cygdrive/c/ghc/gch-6.8.1/bin/ghcii.sh"
        "c:\\Haskell\\2012.2.0.0\\bin\\ghci.exe"))

;;; rc-haskell-mode.el ends here
