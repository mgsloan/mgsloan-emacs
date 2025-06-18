(use-package diminish)
(use-package no-littering)

;; Include path to current file in title, so that it gets picked up by
;; https://github.com/mgsloan/mgsloan-dotfiles/blob/master/env/src/Notes.hs
(setq frame-title-format `((buffer-file-name "%f" "%b")," - Emacs"))

(with-demoted-errors "Error while setting font: %S"
  (set-frame-font (if (getenv "HIDPI") "Hack 10" "Hack 10") nil t))

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
(setq column-number-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

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
  (setq avy-timeout-seconds 0.25)
  :bind
  ("<menu>" . 'avy-goto-word-1)
  ("C-<menu>" . 'avy-goto-char-2))

(use-package evil
  :init
  (setq evil-toggle-key "C-e")
  (setq evil-want-keybinding nil)
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
  (bkevil "M-p" 'my-paste-from-xclipboard))

(use-package spacemacs-theme
  :defer t)

(use-package auto-dark
  :init
  (setq auto-dark-dark-theme 'spacemacs-dark)
  (setq auto-dark-light-theme 'spacemacs-light)
  (setq auto-dark-polling-interval-seconds 5)
  :config (auto-dark-mode t))

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
  (spaceline-toggle-hud-off))

(use-package general
  :config
  (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    "saP" 'helm-projectile-rg
    "sf" 'helm-projectile-find-file
    "gs" 'magit-status
    "gh" 'magit-status-here
    "gl" 'list-repos
    "gr" 'git-link
    "gy" 'git-link-commit
  ))

(use-package helm-projectile
  :defer t)

(use-package helm-rg
  :defer t)

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
  :bind
  (:map helm-map
	;; For some reason this is needed in order to bind tab in terminal, not sure why.
	;; https://github.com/psibi/dotfiles/blob/533c9103c68c4c8ba14aa2f867af4ae591d2ce4c/.emacs.d/init.el#L219
	("<tab>" . helm-execute-persistent-action)
	("C-i" . helm-execute-persistent-action)
	("C-z" . helm-select-action)
  ))

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

  (defun my-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'my-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'my-projectile-invalidate-cache))))

(use-package unfill)

(use-package zoom-frm
  :commands zoom-frm-in zoom-frm-out zoom-frm-unzoom
  :bind
  (("C-x C--" . my-zoom-frm-out)
   ("C-x C-=" . my-zoom-frm-in)
   ("C-x C-0" . zoom-frm-unzoom)))

(defun my-zoom-frm-out ()
  (interactive)
  (zoom-frm-out)
  (zoom-frm-out))

(defun my-zoom-frm-in ()
  (interactive)
  (zoom-frm-in)
  (zoom-frm-in))

(use-package writeroom-mode)

(defun open-files-in-columns (&rest files)
  (delete-other-windows)
  (switch-to-buffer (find-file (car files)))
  (mapc (lambda (file) (progn (split-window-right)
                              (windmove-right)
                              (balance-windows)
                              (switch-to-buffer (find-file file)))) (cdr files)))

(defun find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))
