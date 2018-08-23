(require 'cl)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load the different config files.
(defvar configs
  '("clipboard"
    "global"
    "git"
    "haskell"
    "rust"
    "javascript"
    "straight-submodules")
  "Configuration files that follow the config/foo.el file path format.")

(setq emacs-config-dir (file-name-directory load-file-name))

(loop for name in configs
      do (load (concat emacs-config-dir name ".el")))

;; Put custom-set-variables in a different file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
