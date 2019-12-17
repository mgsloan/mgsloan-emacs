;; (note this isn't actually used)

;; Clear out global keymap, whitelist things I use

(setq default-global-map (copy-tree global-map))
(setq global-map (make-keymap))

(defun whitelist-global-bindings (cmds)
  (loop for cmd in cmds
        do (substitute-key-definition cmd cmd global-map default-global-map)))

;; (substitute-key-definition 'self-insert-command 'self-insert-command global-map default-global-map)

(whitelist-global-bindings
 '(self-insert-command
   delete-window
   execute-extended-command
   minibuffer-keyboard-quit
  ))

(use-global-map global-map)
