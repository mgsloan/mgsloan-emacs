;; TODO: use? https://github.com/bbatsov/emacs.d/blob/master/init.el#L496
;;  (setq undo-tree-history-directory-alist
;;        `((".*" . ,temporary-file-directory)))

;; Newline at end of file, trim trailing whitespace
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Misc options cleaning up GUI mode
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Set user info
(setq user-full-name "Michael Sloan"
      user-mail-address "mgsloan@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; https://github.com/chrisdone/chrisdone-emacs/blob/4039957a9aeca989f69cbea2294a71f0d304b029/config/global.el#L101
(defun set-auto-saves ()
  "Put autosave files (ie #foo#) in one place, *not*
 scattered all over the file system!"
  (defvar autosave-dir
    (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

  (make-directory autosave-dir t)

  (defun auto-save-file-name-p (filename)
    (string-match "^#.*#$" (file-name-nondirectory filename)))

  (defun make-auto-save-file-name ()
    (concat autosave-dir
            (if buffer-file-name
                (concat "#" (file-name-nondirectory buffer-file-name) "#")
              (expand-file-name
               (concat "#%" (buffer-name) "#")))))

  (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  (setq backup-directory-alist (list (cons "." backup-dir))))

(set-auto-saves)

(defun bkevil (keys command)
  (mapc (lambda (m) (define-key m (kbd keys) command))
        (list evil-normal-state-map
              evil-insert-state-map
              evil-motion-state-map
              evil-emacs-state-map
              ;; evil-lisp-state-map
              )))

(use-package evil
  :init
  (setq evil-toggle-key "C-e")
  :config
  (evil-mode 1)
  (bkevil "<left>" 'windmove-left)
  (bkevil "<right>" 'windmove-right)
  (bkevil "<up>" 'windmove-up)
  (bkevil "<down>" 'windmove-down)
  )

(use-package helm
  :diminish helm-mode
  :defer t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  ;; In file find mode, use C-h for removing a dir name.
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)))
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x C-f" . 'helm-find-files)
  ("C-z" . 'helm-mini)
  (:map helm-map
	("<tab>" . helm-execute-persistent-action)
	("C-z" . helm-select-action)
  ))

(use-package winum
  :diminish winmum-mode
  :init
  (setq winum-auto-setup-mode-line nil)
  :config
  (winum-mode t)
  :bind
  ("M-0" . 'winum-select-window-0-or-10)
  ("M-1" . 'winum-select-window-1)
  ("M-2" . 'winum-select-window-2)
  ("M-3" . 'winum-select-window-3)
  ("M-4" . 'winum-select-window-4)
  ("M-5" . 'winum-select-window-5)
  ("M-6" . 'winum-select-window-6)
  ("M-7" . 'winum-select-window-7)
  ("M-8" . 'winum-select-window-8)
  ("M-9" . 'winum-select-window-9)
  )

(use-package persp-mode
  :diminish persp-mode
  :config
  (persp-mode 1))

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config
  (eyebrowse-mode t))

(use-package spacemacs-theme
  :defer t)

(use-package circadian
  :config
  (setq
    calendar-location-name "WA"
    calendar-latitude 47.75
    calendar-longitude -120.74)
  (setq circadian-themes '((:sunrise . spacemacs-light)
                           (:sunset . spacemacs-dark)))
  (circadian-setup))

(use-package spaceline
  :after (winum persp-mode eyebrowse)
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'utf-8)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-hud-off)
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat emacs-config-dir "/undo-history")))))

;; (use-package ag)

;; (use-package wgrep)

;; (use-package helm-ag)

;; (use-package ivy
;;   :diminish
;;   :demand t

;;   :bind
;;   ("C-z" . ivy-switch-buffer))

;; TODO:
;;
;; * Use which key? May not be needed.
;;
;; * Pick some other themes. Zenburn?
;;
;; * Use ido-switch-buffer for C-z like in my old conf?
;;     (bind-key* "C-z" 'ido-switch-buffer)
;;
;; * Opt in to all keybindings via "(use-global-map
;;   (make-sparse-keymap))"?
;;
;; * Revisit http://pragmaticemacs.com/emacs/easily-manage-emacs-workspaces-with-eyebrowse/
;;
;; * Choose between eyebrowse and persp.  Use projectile with persp?
;;
;; * Hippie expansion seems popular
;;
;; * helm-swoop
