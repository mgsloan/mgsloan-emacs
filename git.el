(defun mgsloan-repo-list ()
  (and (string= user-login-name "mgsloan")
       (string= system-name "treetop")
       ;; When GIT_DIR is set, repo list won't work
       (not (getenv "GIT_DIR"))))

(use-package magit
  :init
  (setq
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10
     ;; ask me if I want a tracking upstream
     magit-set-upstream-on-push 'askifnotset)
  :preface
  (defun list-repos ()
    "list my repositories"
    (interactive)
    (progn (magit-list-repositories)
           (with-current-buffer
             (get-buffer "*Magit Repositories*")
             ;; (tabulated-list-sort 2)
             (beginning-of-buffer)
             (current-buffer))))
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident                  ())
          ("Branch"  10 magit-repolist-column-branch                 ())
          ("D"        1 magit-repolist-column-dirty                  ())
          ("L<U"      3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
          ("L>U"      3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
          ("Path"    99 magit-repolist-column-path                   ())))
  (if (mgsloan-repo-list)
      (setq magit-repository-directories
            `(("~/proj/site" . DEPTH0)
              ("~/proj/site/out" . DEPTH0)
              ("~/proj/site/draft" . DEPTH0)
              ("~/proj/apply-unordered" . DEPTH0)
              ("~/proj/nightwriter" . DEPTH0)
              ("~/proj/storepats" . DEPTH0)
              ("~/proj/th-orphans" . DEPTH0)
              ("~/proj/th-reify-many" . DEPTH0)
              ("~/proj/th-utilities" . DEPTH0)
              ("~/proj/todoist-shortcuts" . DEPTH0)
              ("~/proj/unblock-with-intention" . DEPTH0)
              ("~/docs" . DEPTH0)
              ("~/.emacs.d" . DEPTH0))))
  (evil-define-key 'motion magit-repolist-mode-map (kbd "g")
    'tabulated-list-revert))

(if (mgsloan-repo-list)
    (setq initial-buffer-choice 'list-repos))

(use-package evil-magit
  :after (magit evil)
  :config
  ;; Use evil keybindings for all of magit except for magit-status
  (push '("magit:.*" . emacs) evil-buffer-regexps))

;; major mode for editing `git rebase -i` files
;; (use-package rebase-mode)

;; blame mode within buffers
;; (use-package magit-blame-mode)

; Magit status here from https://github.com/magit/magit/issues/2968

(defun magit-status-here--hunk-ranges (hunk)
  "Retrieve the range of a HUNK."
  (let (;; This is the +linenumeber,numlines
        (newrange (caddr (magit-section-value hunk))))
    (string-match "^\\+\\([0-9]+\\),\\([0-9]+\\)$" newrange)
    (let ((rangestart (string-to-number (match-string 1 newrange)))
          (rangesize (string-to-number (match-string 2 newrange))))
      (list hunk rangestart rangesize))))

(defun magit-status-here--file-hunks (section filename)
  "Retrieve the hunks for FILENAME in SECTION."
  (let ((file-section (--first (and (eq (magit-section-type it) 'file)
                                    (equal (magit-section-value it) filename))
                               (magit-section-children section))))
    (when file-section
      (magit-section-children file-section))))

(defun magit-section-see (section)
  "Make a SECTION visible.

Like `magit-section-show' but recursively shows all ancestors as
well, so that SECTION is actually visible.

Point does not move."
  ;; Show all ancestors.  Would be nice to have `magit-section-focus'.
  (let ((s section))
    (while s
      (magit-section-show s)
      (setq s (magit-section-parent s)))))

(defun magit-status-here ()
  "Jump to hunk corresponding to current line in magit."
  (interactive)
  (let ((filename (buffer-file-name))
        (line (line-number-at-pos)))
    (call-interactively #'magit-status)
    (when filename                    ; Guard against non-file-visiting buffers.
      (let* ((filename (file-relative-name filename (magit-toplevel)))
             (unstaged-section (magit-get-section '((unstaged) (status))))
             ;; NOTE: when parts of a file have been staged, the unstaged diff
             ;;       ranges no longer reflect the file ranges.
             ;;
             ;;       This needs more work, as Magit (and probably Git) does
             ;;       not offer enough information (easily) to land at the
             ;;       right spot.
             ;;
             ;;       Nevertheless, users do not usually want to land on the
             ;;       staged section.
             ;;
             ;; (staged-section   (magit-get-section '((staged)   (status))))
             ;; (staged-hunks (and staged-section
             ;;                    (magit-status-here--file-hunks staged-section filename)))
             ;; (hunks (append unstaged-hunks staged-hunks))
             (unstaged-hunks (and unstaged-section
                                  (magit-status-here--file-hunks unstaged-section filename)))
             (hunks unstaged-hunks)
             ;; A list of triples (hunk linestart lineend), the region of each
             ;; hunk.
             (ranges (mapcar #'magit-status-here--hunk-ranges hunks)))
        (when hunks
          ;; The nearest is the one that includes current line, or the one that
          ;; starts or ends nearest to it.
          (let* ((best-fit (--min-by
                            (> (cadr it) (cadr other))
                            (--map
                             (let* ((hunk (car it))
                                    (rangestart (cadr it))
                                    (rangesize (caddr it))
                                    (diff (- line rangestart)))
                               ;; The hunk, the measure and the offset from hunk start.
                               (cond
                                ;; Current line is before hunk.
                                ((< diff 0) (list hunk (abs diff) nil))
                                ;; Current line is after hunk.
                                ((> diff rangesize) (list hunk (- diff rangesize) nil))
                                ;; Inside: it's this!
                                (:else (list hunk 0 diff))))
                             ranges)))
                 (hunk (car best-fit))
                 ;; Offset in the hunk, only if hunk is present. Skip the hunk header.
                 (offset (caddr best-fit))
                 (offset (and offset (1+ offset))))
            ;; Go to the header.
            (magit-section-goto hunk)
            ;; Make it visible.
            (magit-section-see hunk)
            ;; Forward `offset' lines, not counting removed ones.
            (when offset
              (let ((counter 0))
                (while (< counter offset)
                  (unless (string-equal "-" (buffer-substring (point) (1+ (point))))
                    (cl-incf counter))
                  (forward-line))))
            ;; Display hunk in upper part of view -- only when current line
            ;; remains visible.  It looks like with no recenter the cursor does
            ;; not get moved, so always recenter.  That's what we want, anyway.
            ;;
            ;; Again, a `magit-section-focus' function would be pretty helpful,
            ;; as a big hunk should not be centered but aligned with the top
            ;; window.
            (if (and offset
                     (< (1+ offset) (window-body-height)))
                (save-excursion
                  (magit-section-goto hunk)
                  (recenter 0))
              (recenter))))))))

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package smeargle)

;; TODO:
;;
;; * git timemachine
;;
;; * git-link
;;
;; * keybinding for status-here?
;;
;; * Consider some more magit settings from https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;;
;; * magithub?
