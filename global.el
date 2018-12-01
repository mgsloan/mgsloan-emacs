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

;; Enable mouse use in terminal
(xterm-mouse-mode 1)

;; Disable emacs backup and autosave files (surrounded by hashes)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Spaces are better than tabs.
(setq-default indent-tabs-mode nil)

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

  ;; Make word / search include underscores in symbols.
  ;; (modify-syntax-entry ?_ "w")

  ;; TODO: figure out how to do this for * and # without modifying
  ;; other motions.
  )

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

;; Custom keybinding
(use-package general
  :config
  (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    ;; TODO: quickjump or ace jump
    ;; "SPC"
    ;; TODO: Make this not use the "do-" variant when in low-battery use mode

    "saP" 'counsel-projectile-rg
    "sf" 'counsel-projectile-find-file

    ;; TODO: pick a better keybinding.  If you miss the space, this
    ;; replaces a char with l.

    "rl" 'ivy-resume

    "gs" 'magit-status
    "gl" 'git-link
    "gc" 'git-link-commit
    "gb" 'git-link-branch
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
;; * "ivy-avy" command.  Good reason to use it instead of quickjump?
;;
;; * Revisit https://github.com/emacs-evil/evil/blob/master/evil-maps.el
;;
;; * Projectile search that opens result in other buffer
;;
;; * copy-as-format
