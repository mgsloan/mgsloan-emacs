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

;; Different library files to load.
(defvar libs
  '("quickjump")
  "Configuration files that follow the lib/foo.el file path format.")

;; Different config files to load.
(defvar configs
  '("clipboard"
    "global"
    "git"
    "haskell"
    "rust"
    "javascript"
    "markdown")
  "Configuration files that follow the ./foo.el file path format.")

(setq emacs-config-dir (file-name-directory load-file-name))
(setq emacs-lib-dir (concat emacs-config-dir "lib/"))

(loop for name in libs
      do (load (concat emacs-lib-dir name ".el")))

(loop for name in configs
      do (load (concat emacs-config-dir name ".el")))

;; Put custom-set-variables in a different file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
