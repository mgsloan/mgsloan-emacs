(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :bind (:map haskell-mode-map
              ("C-c ," . haskell-navigate-imports)
              ("C-c C-." . haskell-mode-format-imports)
              ("C-c C-u" . my-haskell-insert-undefined))
  :preface
  (defun my-haskell-insert-undefined ()
    (interactive) (insert "undefined"))
  )

;; TODO:
;;
;; * interactive version of haskell-string-literal-encode and decode
