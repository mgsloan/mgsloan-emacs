(use-package diminish)
(use-package no-littering)

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

;; TODO evil has more maps than this https://github.com/noctuid/evil-guide
(defun bkevil (keys command)
  (mapc (lambda (m) (define-key m (kbd keys) command))
        (list evil-normal-state-map
              evil-insert-state-map
              evil-motion-state-map
              evil-emacs-state-map
	      evil-visual-state-map
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
  ;; None of the builtin clipboard stuff seems to work for me, so hack
  ;; in some xsel invocations.
  ;; TODO: get this to work elsewhere than visual mode
  (evil-define-operator my-evil-yank (beg end type register yank-handler)
    "Saves the characters in motion into the kill-ring and xclip-board."
    :move-point nil
    :repeat nil
    (interactive "<R><x><y>")
    (evil-yank beg end type register yank-handler)
    (my-copy-to-xclipboard nil))
  (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)
  (bkevil "M-p" 'my-paste-from-xclipboard)
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
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x C-f" . 'helm-find-files)
  ("C-z" . 'helm-mini)
  :bind
  (:map helm-map
	;; TODO: why doesn't this work?
	("<tab>" . helm-execute-persistent-action)
	;; For some reason this is needed in order to bind tab in terminal, not sure why.
	;; https://github.com/psibi/dotfiles/blob/533c9103c68c4c8ba14aa2f867af4ae591d2ce4c/.emacs.d/init.el#L219
	("C-i" . helm-execute-persistent-action)
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
  (setq undo-tree-auto-save-history t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package helm-ag)

;; Custom keybinding
(use-package general
  :after (helm-ag)
  :config
  (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    ;; TODO: quickjump or ace jump
    ;; "SPC"
    ;; TODO: Make this not use the "do-" variant when in low-battery use mode
    ;; TODO: figure out why it doesn't work.
    ;; "saP" '(helm-do-ag-project-root)
  ))

;; (use-package ag)

;; (use-package wgrep)

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
;;
;; * evil-jumper
;;
;; * Figure out why there are borders around the frame. -ib and -bw don't solve it
;;
;; * Have C-z use less space, use C-S-z for the helm switcher
