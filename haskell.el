;; Sadly not writing much haskel lthese days, not really using haskell-mode.

;; (use-package haskell-mode
;;   :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
;;          ("\\.lhs\\'" . literate-haskell-mode)
;;          ("\\.cabal\\'" . haskell-cabal-mode))
;;   :bind (:map haskell-mode-map
;;               ("C-c ," . haskell-navigate-imports)
;;               ("C-c C-." . haskell-mode-format-imports)
;;               ("C-c C-u" . my-haskell-insert-undefined)
;;               ("M-<left>" . haskell-move-nested-left)
;;               ("M-<right>" . haskell-move-nested-right)
;;         )
;;   :preface
;;   (defun my-haskell-insert-undefined ()
;;     (interactive) (insert "undefined"))
;;   )



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
