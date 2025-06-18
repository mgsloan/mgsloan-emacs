(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)))

(use-package rust-mode
  :demand
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda () (set-fill-column 100))))

(use-package js2-mode
  :defer t
  :mode (("\\.js$" . js2-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js2-indent-switch-body t))

(setq js-indent-level 2)
(setq js-indent-offset 2)

;; Copy-modified from
;; https://github.com/syl20bnr/spacemacs/blob/488e2e3b665365a668ab528014ea7a21a24dd860/layers/%2Blang/markdown/packages.el#L45
(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :defer t)
