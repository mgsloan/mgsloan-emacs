(require 'cl)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(setq use-package-always-ensure t)
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))
(require 'diminish)
(require 'bind-key)

(setq emacs-config-dir (file-name-directory load-file-name))

;; Load the different config files.
(defvar configs
  '("global"
    "git"
    "rust")
  "Configuration files that follow the config/foo.el file path format.")
(loop for name in configs
      do (load (concat emacs-config-dir
                       "config/"
                       name ".el")))

;; Put custom-set-variables in a different file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
