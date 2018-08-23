(use-package rust-mode
  ;; TODO: figure out why demand is needed for it to auto start on .rs files
  :demand
  :mode "\\.rs\\'")

;; TODO: Figure out why racer is so darn slow
;;
;; (use-package racer
;;   :after (company rust-mode)
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   :bind
;;   ("<tab>" . 'company-indent-or-complete-common))

;; (use-package flycheck-rust)
