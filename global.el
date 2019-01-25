(use-package diminish)
(use-package no-littering)

(with-demoted-errors "Error while setting font: %S" (set-frame-font "Hack 16" nil t))

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

;; Enable mouse use in terminal
(xterm-mouse-mode 1)

;; Disable emacs backup and autosave files (surrounded by hashes)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Spaces are better than tabs.
(setq-default indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("dunstrc" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("local.bashrc" . sh-mode))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun px-query-replace-in-open-buffers (arg1 arg2)
  "query-replace in all open files"
  (interactive "sRegexp:\nsReplace with:")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (goto-char (point-min))
       (query-replace-regexp arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))

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

(use-package avy
  :config
  (setq avy-timeout-seconds 0.25))

(use-package evil
  :init
  (setq evil-toggle-key "C-e")
  :config
  (evil-mode 1)
  (bkevil "<left>" 'windmove-left)
  (bkevil "<right>" 'windmove-right)
  (bkevil "<up>" 'windmove-up)
  (bkevil "<down>" 'windmove-down)
  (bkevil "<backspace>" 'evil-delete-backward-char-and-join)
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

  (defun my-return ()
    (interactive)
    (if (eq major-mode 'dired-mode)
        (call-interactively 'dired-find-file)
        (call-interactively 'avy-goto-word-1)))
  (define-key evil-normal-state-map (kbd "<return>") 'my-return)
  (define-key evil-visual-state-map (kbd "<return>") 'my-return)
  (define-key evil-normal-state-map (kbd "C-<return>") 'avy-goto-char-2)
  (define-key evil-visual-state-map (kbd "C-<return>") 'avy-goto-char-2)

  ;; Make word / search include underscores in symbols.
  ;; (modify-syntax-entry ?_ "w")

  ;; TODO: figure out how to do this for * and # without modifying
  ;; other motions.
  )

(use-package evil-mc
  :after evil)

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
    calendar-latitude 47
    calendar-longitude -120)
  (setq circadian-themes '((:sunrise . spacemacs-light)
                           (:sunset . spacemacs-dark)))
  (circadian-setup))

(use-package spaceline
  :after (winum eyebrowse)
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'utf-8)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-hud-off)
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t))

;; TODO: get in the habit of using
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package command-log-mode)

;; Custom keybinding
(use-package general
  :config
  (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    ;; TODO: Make this not use the "do-" variant when in low-battery use mode

    "saP" 'helm-projectile-rg
    "sf" 'helm-projectile-find-file

    ;; TODO: pick a better keybinding.  If you miss the space, this
    ;; replaces a char with l.

    "rl" 'helm-resume

    "gs" 'magit-status
    "gh" 'magit-status-here
    "gl" 'git-link
    "gc" 'git-link-commit
  ))

(use-package helm-projectile)
(use-package helm-rg)

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
  ;; ("C-x C-f" . 'helm-find-files)
  ;; ("C-z" . 'helm-mini)
  :bind
  (:map helm-map
	("<tab>" . helm-execute-persistent-action)
	;; For some reason this is needed in order to bind tab in terminal, not sure why.
	;; https://github.com/psibi/dotfiles/blob/533c9103c68c4c8ba14aa2f867af4ae591d2ce4c/.emacs.d/init.el#L219
	("C-i" . helm-execute-persistent-action)
	("C-z" . helm-select-action)
  ))


(use-package ag)

(use-package wgrep-ag)

(use-package company)

(use-package flycheck)

;; Copied and slightly modified from
;; https://github.com/jwiegley/dot-emacs/blob/e4b5661f72d774fbeeaca6bf900f4cacbba2ba6e/init.el#L2295
(use-package ivy
  :diminish
  :demand t

  :bind ("C-z" . ivy-switch-buffer)
        ("C-S-z" . ivy-switch-buffer-other-window)

  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("C-d"   . ivy-done-or-delete-char)
              ("C-i"   . ivy-partial-or-done)
              ("M-r"   . ivy-reverse-i-search)
	      ("C-h"   . counsel-up-directory)
	      ("C-j"   . ivy-next-line)
	      ("C-k"   . ivy-previous-line-or-history)
	      ("C-l"   . ivy-alt-done))

  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))

  :custom
  ;; (ivy-dynamic-exhibit-delay-ms 100)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-on-del-error-function #'ignore)

  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-immediate-done
       #'ivy-delete-char)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my-ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  ;; https://github.com/abo-abo/swiper/issues/1068#issuecomment-318124861
  ;; FIXME: use
  (defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))


(use-package counsel
  :after ivy
  :demand t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))

  :bind (("M-x" . counsel-M-x))

  ;; from johnw's config
  ;; :bind (("C-*"     . counsel-org-agenda-headlines)
  ;;        ("C-x C-f" . counsel-find-file)
  ;;        ("C-c e l" . counsel-find-library)
  ;;        ("C-c e q" . counsel-set-variable)
  ;;        ("C-h e l" . counsel-find-library)
  ;;        ("C-h e u" . counsel-unicode-char)
  ;;        ("C-h f"   . counsel-describe-function)
  ;;        ("C-x r b" . counsel-bookmark)
  ;;        ;; ("M-y"     . counsel-yank-pop)

  ;;        ("M-s f" . counsel-file-jump)
  ;;        ;; ("M-s g" . counsel-rg)
  ;;        ("M-s j" . counsel-dired-jump))

  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(use-package projectile
  ;; TODO(mgsloan): Why is it defer 5?
  :defer 5
  :diminish
  :bind* ("C-c TAB" . projectile-find-other-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)

  ;; TODO(mgsloan): Figure out what this is all about (from johnw's
  ;; config)

  (defun my-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'my-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'my-projectile-invalidate-cache))))

;; TODO: better keybinding
;; (use-package crosshairs
;;   :init
;;   (bkevil "C-f" 'flash-crosshairs))

(use-package rainbow-delimiters)

(use-package unfill)

(use-package zoom-frm
  :commands zoom-frm-in zoom-frm-out zoom-frm-unzoom
  :bind
  (("C-x C--" . zoom-frm-out)
   ("C-x C-=" . zoom-frm-in)
   ("C-x C-0" . zoom-frm-unzoom)))

;; TODO: This seems to need package.el.  Instead using a custom function.
;; (use-package centered-window :ensure t)
;;
;; TODO: Would be nice to have auto-re-center
;;
;; From https://stackoverflow.com/a/24957203
(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))

;; Doesn't work well with multiple frames + non ideal efficiency
;;
;; (use-package keyfreq
;;   :config
;;   ;; Omit some things that get spammed
;;   (setq keyfreq-excluded-commands
;;         '(self-insert-command
;;           mwheel-scroll
;;           mouse-drag-region
;;           mouse-set-point
;;           ivy-done
;;           evil-backward-paragraph
;;           evil-forward-paragraph
;;           evil-next-line
;;           evil-previous-line))
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))


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
;; * evil-jumper
;;
;; * Figure out why there are borders around the frame. -ib and -bw don't solve it
;;
;; * Figure out why ivy-rich doesn't work for me
;;
;; * multiple cursors mode
;;
;; * persp-mode
;;
;; * swiper
;;
;; * ivy-avy
;;
;; * ivy-isearch
;;
;; * Revisit https://github.com/emacs-evil/evil/blob/master/evil-maps.el
;;
;; * Projectile search that opens result in other buffer
;;
;; * copy-as-format
;;
;; * Prevent numeric prefix of insertion, similar vim commands from
;;   replicating affect.
;;
;; * habit of using evil-mc
