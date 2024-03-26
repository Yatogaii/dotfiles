(use-package! dape
;;  :preface
;;  ;; By default dape shares the same keybinding prefix as `gud'
;;  ;; If you do not want to use any prefix, set it to nil.
;;  ;; (setq dape-key-prefix "\C-x\C-a")
;;
;;  :hook
;;  ;; Save breakpoints on quit
;;  ;; ((kill-emacs . dape-breakpoint-save)
;;  ;; Load breakpoints on startup
;;  ;;  (after-init . dape-breakpoint-load))
;;
;;  :init
;;  ;; To use window configuration like gud (gdb-mi)
;;  ;; (setq dape-buffer-window-arrangement 'gud)
;;
;;  :config
;;  ;; Info buffers to the right
;;  ;; (setq dape-buffer-window-arrangement 'right)
;;
;;  ;; Global bindings for setting breakpoints with mouse
;;  ;; (dape-breakpoint-global-mode)
;;
;;  ;; To not display info and/or buffers on startup
;;  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
;;  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)
;;
;;  ;; To display info and/or repl buffers on stopped
;;  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
;;  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)
;;
;;  ;; Kill compile buffer on build success
;;  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)
;;
;;  ;; Save buffers on startup, useful for interpreted languages
;;  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
;;
;;  ;; Projectile users
;;  ;; (setq dape-cwd-fn 'projectile-project-root)
)

(add-to-list 'dape-configs
 `(debugpy-remote-attach
   modes (python-mode python-ts-mode)
   host (lambda () (read-string "Host: " "localhost"))
   port (lambda () (read-number "Port: "))
   :request "attach"
   :type "python"
   :pathMappings [(:localRoot (lambda ()
                                (read-directory-name "Local source directory: "
                                                     (funcall dape-cwd-fn)))
                   :remoteRoot (lambda ()
                                 (read-string "Remote source directory: ")))]
   :justMyCode nil
   :showReturnValue t))
