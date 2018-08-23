;; I posted this code here:
;; https://github.com/raxod502/straight.el/issues/301#issuecomment-412761572

(defun straight-add-submodule (package)
  (interactive (list (straight--select-package "Add submodule" nil 'installed)
                     current-prefix-arg))
  (let ((recipe (gethash package straight--recipe-cache)))
    (straight-vc-add-submodule recipe)))

(defun straight-add-submodule-all (predicate)
  (interactive "P")
  (straight--map-existing-repos-interactively
   (lambda (package)
     (straight-add-submodule package))
   predicate))

(defun straight-vc-add-submodule (recipe)
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir)))
        (straight-vc 'add-submodule type recipe)))))

(cl-defun straight-vc-git-add-submodule (recipe)
  (straight--with-plist recipe
      (local-repo repo host)
    (let ((url (straight-vc-git--encode-url repo host)))
      (unless (straight--check-call "git" "ls-files" "--error-unmatch" local-repo)
        (straight--get-call "git" "submodule" "add" url local-repo)))))
