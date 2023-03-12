(use-package js2-mode
  :defer t
  :mode (("\\.js$" . js2-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js2-indent-switch-body t))

(setq js-indent-level 2)
(setq js-indent-offset 2)
