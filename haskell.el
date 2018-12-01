(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook (lambda () (setq-local eldoc-documentation-function nil))))
