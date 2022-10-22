;; Sadly not writing much haskel lthese days, not really using haskell-mode.

(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)))


;; Old commented out stuff

;; (use-package lsp-mode
;;   :commands lsp)

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))

;; (use-package lsp-haskell)
  ;; :hook (haskell-mode . lsp))

;; TODO:
;;
;; * interactive version of haskell-string-literal-encode and decode
