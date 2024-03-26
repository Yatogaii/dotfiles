(use-package! evil
  :init (progn
          (setq evil-want-integration nil)
          )
)

(use-package! evil-escape
  :config (progn
            (setq-default evil-escape-key-sequence (kbd "jk"))
            (setq-default evil-escape-delay 0.2)

            (evil-escape-mode t)

            ;; 通过evil-escape退出evil-god-state/关闭citre-peek-mode的窗口
            (defun zjy/evil-escape--escape-normal-state (orig-fn &rest args)
              (cond
               ((bound-and-true-p citre-peek--mode) 'citre-peek-abort)
               ((eq evil-state 'god) 'evil-god-state-bail)
               (t
                (apply orig-fn args))))
            (advice-add #'evil-escape--escape-normal-state :around #'zjy/evil-escape--escape-normal-state)

            (defun zjy/evil-escape-p (orig-fn &rest args)
              (or (and
                   evil-escape-key-sequence
                   (not evil-escape-inhibit)
                   (bound-and-true-p citre-peek--mode)
                   )
                  (and
                   evil-escape-key-sequence
                   (not evil-escape-inhibit)
                   (eq evil-state 'god)
                   )
                  (apply orig-fn args))
              )
            (advice-add #'evil-escape-p :around #'zjy/evil-escape-p)

            )
  )

;; god-mode不需要特殊配置

(use-package! evil-god-state
 ; :after (god-mode evil)
  :config (progn
            ;; 按空格进入evil-god-state
            (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
            (evil-define-key 'god global-map [escape] 'evil-god-state-bail)

            (with-eval-after-load 'diminish
              (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
              (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
              )

            (with-eval-after-load 'which-key
              (which-key-enable-god-mode-support)
              )

            ;;(with-eval-after-load 'pyim
            ;;  (defvar-local zjy/evil-in-god-mode nil)
            ;;  (add-hook 'evil-god-state-entry-hook (lambda() (setq-local zjy/evil-in-god-mode t)))
            ;;  (add-hook 'evil-god-state-exit-hook (lambda () (setq-local zjy/evil-in-god-mode nil)))
            ;;  
            ;;  (defun zjy/pyim-probe-evil-god-mode ()
            ;;    zjy/evil-in-god-mode
            ;;    )
            ;;  ;; 进入god-mode时，自动进入英文模式，这样pyim不会将后续的输入作为拼音。
            ;;  (add-to-list 'pyim-english-input-switch-functions 'zjy/pyim-probe-evil-god-mode)
            ;;  )

            )
  )
