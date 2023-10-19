(use-package! eglot
  :ensure t
  :hook (( prog-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0)
  (advice-add 'jsonrpc--log-event :around
              (lambda (_orig-func &rest _))))


(setq lsp-log-io t)
(with-eval-after-load 'lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                     :major-modes '(c-mode c++-mode objc-mode)
                     :remote? t
                     :server-id 'clangd-remote)))
