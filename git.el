;;; Home dotfiles repo
;;
;; ~/.home.git is the git dir for a repo whose work tree is $HOME, and there is
;; deliberately no ~/.git - a plain `git' run under $HOME must not find it (see
;; ~/env/home-dir-git.md).  Setting GIT_DIR process-wide, as the old `edit_cfg'
;; script did, makes it findable but leaves Emacs unable to open any other repo,
;; and requires patching `magit-startup-asserts', which exists to prevent
;; exactly that.  Instead, inject the vars per git invocation, for the
;; directories that belong to the repo.
;;
;; `magit-process-environment' is the single chokepoint: every git subprocess
;; magit runs, synchronous and asynchronous alike, gets its environment from
;; there.

(defconst my-home-work-tree (file-name-as-directory (expand-file-name "~")))
(defconst my-home-git-dir   (expand-file-name ".home.git/" my-home-work-tree))
(defconst my-home-env-dir   (expand-file-name "env/" my-home-work-tree))

(defconst my-home-repo-name "env"
  "What to call the home repo wherever magit displays a repository name.
Its actual basename is the username, which is meaningless in a repo list.")

(defvar my-home-repo--tracked-dirs nil
  "Hash table of every directory containing tracked home-repo content.")

(defun my-home-repo-tracked-dirs (&optional refresh)
  "Directories that contain content tracked by the home dotfiles repo.
Built from a single `git ls-files' and cached, so that
`my-home-repo-dir-p' costs no subprocess."
  (when (or refresh (null my-home-repo--tracked-dirs))
    (let ((table (make-hash-table :test #'equal))
          (default-directory my-home-work-tree))
      ;; Deliberately `call-process' with explicit flags rather than any magit
      ;; function, so this can never recurse back into `my-home-repo-dir-p'.
      (with-temp-buffer
        (when (eq 0 (call-process "git" nil t nil
                                  (concat "--git-dir=" my-home-git-dir)
                                  (concat "--work-tree=" my-home-work-tree)
                                  "ls-files" "-z"))
          (dolist (file (split-string (buffer-string) "\0" t))
            (let ((dir (file-name-directory
                        (expand-file-name file my-home-work-tree))))
              (while (and dir (>= (length dir) (length my-home-work-tree)))
                (puthash dir t table)
                (setq dir (file-name-directory (directory-file-name dir))))))))
      (setq my-home-repo--tracked-dirs table)))
  my-home-repo--tracked-dirs)

(defun my-home-repo-refresh ()
  "Rescan which directories the home dotfiles repo tracks.
Needed after tracking a new top-level entry outside env/."
  (interactive)
  (my-home-repo-tracked-dirs t))

(defun my-home-repo--no-nearer-git-p (dir)
  "Whether no `.git' exists at or above DIR, stopping at $HOME.
`file-exists-p' follows symlinks, which is what we want: a symlinked-in
checkout is recognized as its own repository."
  (let ((d dir))
    (catch 'found
      (while (> (length d) (length my-home-work-tree))
        (when (file-exists-p (expand-file-name ".git" d))
          (throw 'found nil))
        (setq d (file-name-directory (directory-file-name d))))
      t)))

(defun my-home-repo-dir-p (dir)
  "Whether git run in DIR should be pointed at the home dotfiles repo."
  (and (stringp dir)
       ;; Must come first: `magit-process-environment' returns early for remote
       ;; directories, and a local GIT_DIR would break every Tramp git call.
       (not (file-remote-p dir))
       (let ((dir (file-name-as-directory (expand-file-name dir))))
         (cond
          ;; The control directory and everything under it.  Required for
          ;; committing and rebasing to work at all: COMMIT_EDITMSG, MERGE_MSG
          ;; and git-rebase-todo buffers have their `default-directory' here.
          ((string-prefix-p my-home-git-dir dir) t)
          ((not (string-prefix-p my-home-work-tree dir)) nil)
          ;; A real repo nested under $HOME always wins - ~/.emacs.d, ~/proj/*,
          ;; and this repo's own submodules.
          ((not (my-home-repo--no-nearer-git-p dir)) nil)
          ;; Under env/, untracked files and brand-new directories count too, so
          ;; that a freshly created env/foo/bar.sh is stageable right away.
          ((string-prefix-p my-home-env-dir dir) t)
          ;; Elsewhere require tracked content, so that ~/proj, ~/dl and friends
          ;; keep reporting no repository, exactly as they do without any of
          ;; this.  $HOME itself qualifies, being an ancestor of every file.
          (t (and (gethash dir (my-home-repo-tracked-dirs)) t))))))

(defun my-home-repo-toplevel-p (dir)
  "Whether DIR is the home dotfiles repo's work tree root."
  (and (stringp dir)
       (equal (file-name-as-directory (expand-file-name dir))
              my-home-work-tree)))

(defun my-home-git-environment (env)
  "Point ENV at the home dotfiles repo when `default-directory' belongs to it.
Both vars are injected: `core.worktree' is deliberately not set in
~/.home.git/config, so that a stray GIT_DIR with the wrong working
directory still fails loudly rather than operating on all of $HOME."
  (if (my-home-repo-dir-p default-directory)
      (cons (concat "GIT_DIR=" my-home-git-dir)
            (cons (concat "GIT_WORK_TREE=" my-home-work-tree) env))
    env))

;; `magit-list-repos-1' requires a readable <dir>/.git, so the home repo cannot
;; be expressed through `magit-repository-directories' at any depth.
(defun my-magit-list-repos-add-home (repos)
  (if (file-directory-p my-home-git-dir)
      (cons my-home-work-tree repos)
    repos))

;; Magit names a repo after the basename of its toplevel, which for the home
;; repo is the username.  Rename it in the three places that show it: the repo
;; list, `magit-status' completion, and buffer names.
(defun my-magit-repolist-column-ident (spec)
  (if (my-home-repo-toplevel-p default-directory)
      my-home-repo-name
    (magit-repolist-column-ident spec)))

(defun my-magit-repos-alist-rename-home (alist)
  (mapcar (lambda (cell)
            (if (my-home-repo-toplevel-p (cdr cell))
                (cons my-home-repo-name (cdr cell))
              cell))
          alist))

(defun my-magit-generate-buffer-name (mode &optional value)
  "Like `magit-generate-buffer-name-default-function', but name the home repo.
Mirrors that function rather than advising it, because the repository
name is baked into `magit-buffer-name-format' expansion."
  (if (my-home-repo-toplevel-p default-directory)
      (let ((m (substring (symbol-name mode) 0 -5))
            (v (and value (format "%s" (ensure-list value)))))
        (format-spec magit-buffer-name-format
                     `((?m . ,m)
                       (?M . ,(if (eq mode 'magit-status-mode) "magit" m))
                       (?v . ,(or v ""))
                       (?V . ,(if v (concat " " v) ""))
                       (?t . ,my-home-repo-name)
                       (?x . ,(if magit-uniquify-buffer-names "" "*")))))
    (magit-generate-buffer-name-default-function mode value)))

(defun my-magit-repolist-column-branch (_id)
  "Like `magit-repolist-column-branch', but not fooled by a separate gitdir.
The upstream `.git' check keeps an unpopulated submodule from reporting
its parent's branch; the home repo has no `.git' but is populated."
  (if (or (file-exists-p ".git")
          (my-home-repo-dir-p default-directory))
      (let ((branch (magit-get-current-branch)))
        (if (member branch magit-main-branch-names)
            (magit--propertize-face branch 'shadow)
          branch))
    (magit--propertize-face "(unpopulated)" 'warning)))

(defun my-magit-mark-home-repo ()
  "Make the home dotfiles repo's status buffer unmistakable."
  (when (my-home-repo-toplevel-p default-directory)
    (setq header-line-format
          (propertize " HOME DOTFILES REPO " 'face 'warning))))

(defun mgsloan-repo-list ()
  (and (string= user-login-name "mgsloan")
       (not (getenv "SUPPRESS_REPO_LIST"))))

;; The repo list used to be a literal list of paths, which went stale every time
;; ~/proj got reorganized. Instead, generate it from the filesystem: the home
;; repo's submodules, everything under ~/proj, and a couple of fixed entries.

(defvar my-repo-scan-roots '("~/proj")
  "Directories scanned recursively for git repositories.")

(defvar my-repo-extra-dirs '("~/docs" "~/.emacs.d")
  "Git repositories always included in `magit-repository-directories'.")

(defvar my-repo-scan-max-depth 4
  "How far below a `my-repo-scan-roots' entry to look for repositories.")

(defvar my-repo-scan-prune-names
  '(".git" "node_modules" ".stack-work" "dist-newstyle" "target")
  "Directory names never descended into while scanning for repositories.
These are all build/dependency dirs, and skipping them is what keeps the
scan cheap - an unbounded walk of ~/proj visits over 400k directories.")

(defun my-git-repo-p (dir)
  "Whether DIR is the root of a git repository."
  (file-exists-p (expand-file-name ".git" dir)))

(defun my-git-submodule-paths (dir)
  "Absolute paths of the submodules declared in DIR's .gitmodules.

Parsed directly rather than via `git config -f' so that this costs no
subprocess, and so that it works regardless of GIT_DIR - which matters
for the home directory repo, whose git dir is ~/.home.git."
  (let ((file (expand-file-name ".gitmodules" dir))
        paths)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*path[ \t]*=[ \t]*\\(.+?\\)[ \t]*$" nil t)
          (push (expand-file-name (match-string 1) dir) paths))))
    (nreverse paths)))

(defun my-scan-repo-dirs (root)
  "Git repositories at most `my-repo-scan-max-depth' below ROOT.

Descends past a repository to find ones nested inside it, but skips any
nested repository listed in its parent's .gitmodules - those are vendored
dependencies rather than things worth seeing in the repo list."
  (let (repos)
    (letrec
        ((walk
          (lambda (dir depth skip)
            (unless (member dir skip)
              (when (my-git-repo-p dir)
                (push dir repos)
                (setq skip (append (my-git-submodule-paths dir) skip)))
              (when (> depth 0)
                (dolist (f (ignore-errors
                             (directory-files
                              dir t directory-files-no-dot-files-regexp t)))
                  (when (and (file-directory-p f)
                             (not (file-symlink-p f))
                             (not (member (file-name-nondirectory f)
                                          my-repo-scan-prune-names)))
                    (funcall walk f (1- depth) skip))))))))
      (funcall walk (directory-file-name (expand-file-name root))
               my-repo-scan-max-depth nil))
    repos))

(defun my-magit-repository-directories ()
  "Value for `magit-repository-directories', generated from the filesystem."
  (let ((dirs (append (my-git-submodule-paths "~")
                      (mapcan #'my-scan-repo-dirs my-repo-scan-roots)
                      (mapcar (lambda (d)
                                (directory-file-name (expand-file-name d)))
                              my-repo-extra-dirs))))
    (mapcar (lambda (dir) (cons dir 0))
            (sort (delete-dups (seq-filter #'my-git-repo-p dirs)) #'string<))))

(defun my-refresh-magit-repository-directories (&rest _)
  "Regenerate `magit-repository-directories'."
  (setq magit-repository-directories (my-magit-repository-directories)))

(use-package
  magit
  :init (setq
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
         magit-set-upstream-on-push 'askifnotset
         ;; word-level diffs
         magit-diff-refine-hunk (quote all))
  :preface (defun list-repos ()
             "list my repositories"
             (interactive)
             (with-current-buffer (get-buffer-create "*Magit Repositories*")
               (magit-list-repositories)
               (beginning-of-buffer)
               (current-buffer)))
  (defun magit-repolist-column-iso-date (_id)
    "date column in iso 8601 format"
    (magit-git-string "log" "-1" "--format=%ci"))
  (defun magit-repolist-column-relative-date (_id)
    "timestamp relative to current time"
    (magit-git-string "log" "-1" "--format=%cr"))
  :config
  ;; Home dotfiles repo: see the section at the top of this file.
  (advice-add 'magit-process-environment :filter-return #'my-home-git-environment)
  (advice-add 'magit-list-repos :filter-return #'my-magit-list-repos-add-home)
  (advice-add 'magit-repos-alist :filter-return #'my-magit-repos-alist-rename-home)
  (setq magit-generate-buffer-name-function #'my-magit-generate-buffer-name)
  (add-hook 'magit-status-mode-hook #'my-magit-mark-home-repo)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  ; todo! these broke?
  ; (add-hook 'git-diff-mode-hook #'my-wrap-lines)
  ; (add-hook 'magit-mode-hook #'my-wrap-lines)
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill
            ;; append to end of git-commit-setup-hook to ensure this hook takes precedence.
            t)
  (setq magit-repolist-columns '(("Name"     25 my-magit-repolist-column-ident               ())
                                 ("D"         1 magit-repolist-column-flag                   ())
                                 ("L<U"       3 magit-repolist-column-unpulled-from-upstream
                                  ((:right-align t)))
                                 ("L>U"       3 magit-repolist-column-unpushed-to-upstream
                                  ((:right-align t)))
                                 ("Date"     14 magit-repolist-column-iso-date               ())
                                 ("Modified" 16 magit-repolist-column-relative-date
                                  ((:right-align t)))
                                 ("Branch"   10 my-magit-repolist-column-branch              ())
                                 ("Path"     99 magit-repolist-column-path                   ())))
  (when (mgsloan-repo-list)
    ;; `magit-list-repos' is the one place both the repo list buffer (initial
    ;; display and `g' refresh) and `magit-status' read this variable through,
    ;; so regenerating here keeps every consumer up to date.
    (advice-add 'magit-list-repos :before
                #'my-refresh-magit-repository-directories)
    (my-refresh-magit-repository-directories))
  (evil-define-key 'motion magit-repolist-mode-map (kbd "g") 'tabulated-list-revert)
  (defun magit-add-unstaged-to-misc ()
    "Run `add-unstaged-to-misc` in the current Magit repository directory."
    (interactive)
    (let ((repo-dir (magit-toplevel)))
      (if repo-dir
          (let ((default-directory repo-dir))
            (async-shell-command "~/.local/bin/zed-dev/add-unstaged-to-misc"))
        (message "Not in a Git repository"))))
  (defun magit-add-unstaged-to-todos ()
    "Run `add-unstaged-to-todos` in the current Magit repository directory."
    (interactive)
    (let ((repo-dir (magit-toplevel)))
      (if repo-dir
          (let ((default-directory repo-dir))
            ;; Replace this with your specific CLI command
            (async-shell-command "~/.local/bin/zed-dev/add-unstaged-to-todos"))
        (message "Not in a Git repository"))))
  (defun magit-run-branch-cleaner()
    "Run `local-branch-cleaner` in the current Magit repository directory."
    (interactive)
    (let ((repo-dir (magit-toplevel)))
      (if repo-dir
          (let ((default-directory repo-dir))
            ;; Replace this with your specific CLI command
            (async-shell-command "~/proj/utils/local-branch-cleaner/start.sh"))
        (message "Not in a Git repository")))))

; (defun my-wrap-lines ()
;  "Disable `truncate-lines' in the current buffer."
;  (setq truncate-lines nil))

;; Open every repository's status buffer in the background, so that entering
;; any repo from the list is instant.  Deferred: nothing happens until Emacs
;; has been idle for `my-open-all-repos-idle-delay' after startup, and then
;; repositories open one per idle-timer tick so input stays responsive.

(defvar my-open-all-repos-idle-delay 1
  "Idle seconds after startup before repositories start opening.")

(defvar my-open-all-repos--queue nil
  "Repositories still waiting to be opened by `my-open-all-repositories'.")

(defun my-open-all-repositories ()
  "Create a magit-status buffer for every known repository.
The buffers are not displayed, and repositories that already have a
status buffer are skipped."
  (interactive)
  (setq my-open-all-repos--queue (magit-list-repos))
  (my-open-all-repos--continue))

(defun my-open-all-repos--continue ()
  (if-let* ((dir (pop my-open-all-repos--queue)))
      (progn
        (let ((default-directory (file-name-as-directory dir)))
          (unless (magit-get-mode-buffer 'magit-status-mode)
            (condition-case err
                ;; Create the buffer but leave the selected window alone.
                (let ((magit-display-buffer-noselect t)
                      (magit-display-buffer-function #'ignore))
                  (magit-status-setup-buffer default-directory))
              (error (message "my-open-all-repositories: %s: %s"
                              dir (error-message-string err))))))
        (run-with-idle-timer 0.1 nil #'my-open-all-repos--continue))
    (message "Opened all repositories")))

(when (mgsloan-repo-list)
  (setq initial-buffer-choice 'list-repos)
  ;; By `emacs-startup-hook' time the repo list from `initial-buffer-choice'
  ;; has already been rendered, so this only ever runs after it.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer my-open-all-repos-idle-delay nil
                                   #'my-open-all-repositories))))

(use-package evil-collection
  :after (magit evil)
  :config
  ;; Use evil keybindings for all of magit except for magit-status
  (push '("magit:.*" . emacs) evil-buffer-regexps)
  (evil-collection-init))

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
  (setq git-link-use-commit t)
  ;; git-link shells out directly rather than through magit, so it needs the
  ;; home dotfiles repo's environment injected separately.
  (advice-add 'git-link--exec :around
              (lambda (fn &rest args)
                (let ((process-environment
                       (my-home-git-environment process-environment)))
                  (apply fn args)))))

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
