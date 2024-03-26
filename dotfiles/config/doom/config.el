;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; set some CONSTANT dir
;(setq yt-org-base-dir "~/OneDrive/Orgs")
(setq yt-rime-base-dir "~/OneDrive")
(setq rime-user-data-dir "~/OneDrive/RimeConfig/rime-settings")

(setq word-wrap-by-category t)

;; CN mirrors
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org-cn". "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; font setting
;;(setq doom-font (font-spec :family "FiraCode Nerd Font" :weight 'medium :size 13.0))
(setq doom-font (font-spec :family "Source Code Pro" :weight 'medium :size 14.0))

;;(setq doom-font (font-spec :family "0xProto Nerd Font" :weight 'medium :size 14.0))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono"))))

(add-hook 'after-setting-font-hook #'my-cjk-font)

;; 解决粘贴中文出现乱码的问题
(if (eq system-type 'windows-nt)
	(progn
	  ;; (setq selection-coding-system 'utf-16le-dos) ;; 修复从网页剪切文本过来时显示 \nnn \nnn 的问题
	  ;; (set-default selection-coding-system 'utf-16le-dos)
	  (set-selection-coding-system 'utf-16le-dos) ;; 别名set-clipboard-coding-system
	  )
  (set-selection-coding-system 'utf-8))


;(setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'doom-one-light)

(setq display-line-numbers-type t)

;(setq org-directory yt-org-base-dir)
;(use-package! emacsql-sqlite-builtin)

;; End of org-roam

;; Start of Citar

(use-package! citar
  :custom
  (citar-bibliography '(concat yt-org-base-dir "/虚拟化安全.bib")))

;; End of citar


; auto indented
(setq org-startup-indented t)
(setq org-odd-levels-only nil)

;; org-roam-config

;; my new function
  ; jj to toggle IM
  (defun my-toggle-input-method-in-insert-mode ()
    "Toggle input method in insert mode using 'jj'."
    (interactive)
    (when (evil-insert-state-p)
      (toggle-input-method)))
  (define-key evil-insert-state-map (kbd "s-j") 'my-toggle-input-method-in-insert-mode)

  ; my id generator
  (setq ffap-url-regexp "\\(www\\.\\|https?://\\|ftp://\\|file://\\|news:\\|telnet://\\|gopher://\\|wais://\\|mailto:\\|file://\\)\\S")
  (defun my-id-get-or-generate()
  "Returns the ID property if set or generates and returns a new one if not set.
  The generated ID is stripped off potential progress indicator cookies and
  sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
  if none was found before."
      (interactive)
          (when (not (org-id-get))
              (progn
                (let* (
                        (my-heading-text (nth 4 (org-heading-components)));; retrieve heading string
                        (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" my-heading-text));; remove progress indicators like "[25%]"
                        (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" my-heading-text));; remove progress indicators like "[2/7]"
                        (my-heading-text (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" my-heading-text));; remove priority indicators like "[#A]"
                        (my-heading-text (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" my-heading-text t));; removes links, keeps their description and ending brackets
  ;;                      (my-heading-text (replace-regexp-in-string "[<\\[][12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)[>\\]]" "" my-heading-text t));; removes day of week and time from date- and time-stamps (doesn't work somehow)
                        (my-heading-text (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" my-heading-text t));; removes day of week and time from active date- and time-stamps
                        (my-heading-text (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" my-heading-text t));; removes day of week and time from inactive date- and time-stamps
                        (new-id (my-generate-sanitized-alnum-dash-string my-heading-text));; get slug from heading text
                        (my-created-property (assoc "CREATED" (org-entry-properties))) ;; nil or content of CREATED time-stamp
                      )
                    (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
                            ;; only if no ISO date-stamp is found at the beginning of the new id:
                            (if my-created-property (progn
                                ;; prefer date-stamp of CREATED property (if found):
                                (setq my-created-datestamp (substring (org-entry-get nil "CREATED" nil) 1 11)) ;; returns "2021-12-16" or nil (if no CREATED property)
                                (setq new-id (concat my-created-datestamp "-" new-id))
                            )
                            ;; use today's date-stamp if no CREATED property is found:
                            (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id))))
                    (org-set-property "ID" new-id)
                    )
                  )
          )
          (kill-new (concat "id:" (org-id-get)));; put ID in kill-ring
          (org-id-get);; retrieve the current ID in any case as return value
  )

  ;org-mode 插入粘贴板上的图片
(defun yt/org-insert-image ()
  "Insert an image from clipboard into a folder named after the current Org file with BEGIN_IMAGE and END_IMAGE tags."
  (interactive)
  (let* ((org-file-buffer (buffer-name))
         (org-file-name (file-name-base org-file-buffer)) ; 获取当前 Org 文件的文件名
         (path (concat default-directory org-file-name "\\")) ; 创建与 Org 文件同名的文件夹
         (image-file (concat
                      path
                      (format-time-string "%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (make-directory path t)) ; 如果文件夹不存在，创建它
    (shell-command-to-string (concat "powershell.exe -command \""
                                     "$img = Get-Clipboard -Format Image; "
                                     "$img.Save('" image-file "');\""))
    ;; 插入 BEGIN_IMAGE 标记
    (insert "#+BEGIN_IMAGE\n")
    ;; 插入图片链接
    (org-insert-link nil
                     (concat "file:" image-file)
                     "")
    ;; 插入 END_IMAGE 标记
    (insert "\n#+END_IMAGE\n")
    (message image-file)
    (org-display-inline-images)))


  ; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "DOING(i!)" "BLOCK(b!)" "|" "DONE(d!)" "CANCEL(e@/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" .   (:foreground "red" :weight bold))
          ("BLOCK" .   (:foreground "purple" :weight bold))
          ("DOING" .      (:foreground "orange" :weight bold))
          ("DONE" .      (:foreground "green" :weight bold))
          ("CANCEL" .     (:background "gray" :foreground "black"))
          ))

  ; START rime config
  (use-package! rime
    :custom
   ; Linux dont need extra config
   ; (rime-librime-root "~/.config/emacs/librime/dist")
   ; (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@30/30.0.50/include")
    (default-input-method "rime")
    :bind
    (:map rime-active-mode-map
    ;("<tab>" . 'rime-inline-ascii)))
    ))
  ; emacs-rime with evil-escape
  (defun rime-evil-escape-advice (orig-fun key)
  "advice for `rime-input-method' to make it work together with `evil-escape'.
        Mainly modified from `evil-escape-pre-command-hook'"
  (if rime--preedit-overlay
      ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
      (apply orig-fun (list key))
    (when (featurep 'evil-escape)
      (let (
            (fkey (elt evil-escape-key-sequence 0))
            (skey (elt evil-escape-key-sequence 1))
            )
        (if (or (char-equal key fkey)
                (and evil-escape-unordered-key-sequence
                     (char-equal key skey)))
            (let ((evt (read-event nil nil evil-escape-delay)))
              (cond
               ((and (characterp evt)
                     (or (and (char-equal key fkey) (char-equal evt skey))
                         (and evil-escape-unordered-key-sequence
                              (char-equal key skey) (char-equal evt fkey))))
                (evil-repeat-stop)
                (evil-normal-state))
               ((null evt) (apply orig-fun (list key)))
               (t
                (apply orig-fun (list key))
                (if (numberp evt)
                    (apply orig-fun (list evt))
                  (setq unread-command-events (append unread-command-events (list evt))))))
              )
          (apply orig-fun (list key)))))))
  (advice-add 'rime-input-method :around #'rime-evil-escape-advice)
  
  ; bindkey to input and escape
  (defun rime-commit1-and-evil-normal ()
    "Commit the 1st item if exists, then go to evil normal state."
    (interactive)
    (rime-commit1)
    (evil-normal-state))

  ; 自动英文预测
  (setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p
       ; rime-predicate-space-after-ascii-p ; 英文后带空格 
        rime-predicate-space-after-cc-p 
        rime-predicate-current-uppercase-letter-p 
        rime-predicate-tex-math-or-command-p))

  ; END emacs-rime

  ; suppress starting "look" process
  (advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))

;; CORFU Start

(use-package! yasnippet
  :diminish yas-minor-mode
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook (after-init . yas-global-mode))

;(use-package! corfu
;  :custom
;  (corfu-cycle t)
;  (corfu-auto t)
;  (corfu-auto-prefix 1)
;  (corfu-auto-delay 0.1)
;  (corfu-preselect 'prompt)
;  (corfu-on-exact-match nil)
;  :bind (:map corfu-map
;              ([tab] . corfu-next)
;              ([backtab] . corfu-previous)
;              ("s-<return>" . corfu-insert)
;              ([remap move-end-of-line] . nil))
;  :hook (eshell-mode . (lambda () (setq-local corfu-auto nil)))
;  :init  
;  (setq corfu-global-mode t)          ;; Move this line here
;  (setq corfu-popupinfo-mode t))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (when (eq system-type 'windows-nt)
    (plist-put kind-icon-default-style :height 0.8))
  (when (fboundp 'reapply-themes)
    (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(use-package! corfu
   :after orderless
   :custom
   (corfu-quit-at-boundary nil)
   (corfu-quit-no-match t)
   (corfu-cycle t)
   (corfu-auto t)
 :bind (:map corfu-map
             ([tab] . corfu-next)
             ([backtab] . corfu-previous)
             ("s-<return>" . corfu-insert)
             ([remap move-end-of-line] . nil))
 :hook (eshell-mode . (lambda () (setq-local corfu-auto nil)))
   :init
   (global-corfu-mode))


(use-package! orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  ;; Fix completing hostnames when using /ssh:
  (setq completion-styles '(orderless)))

(use-package! lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; CORFU End

;; Codeium
;; we recommend using use-package to organize your init.el
(use-package! codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    ;;(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;;(add-hook 'python-mode-hook
    ;;    (lambda ()
    ;;        (setq-local completion-at-point-functions
    ;;            (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; Cape complete backend
(use-package! cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion funcons takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Start LSP
;; 配置 lsp-mode
;(load! "modules/completions/eglot.el")
;; End LSP

;; tab bar
;(load! "modules/ui/tabbar.el")


;; acutex and pdf-tools
(load! "modules/latex/latex.el")

;; Start Lsp-bridge
;(require 'lsp-bridge)
;(use-package! lsp-bridge
;  :config
;  (map! :map acm-mode-map
;        [tab]           #'acm-select-next
;        [backtab]       #'acm-select-prev)
;  (map! :map doom-leader-code-map
;        :desc "LSP Rename"
;        "r"             #'lsp-bridge-rename
;        :desc "LSP Find declaration"
;        "j"             #'lsp-bridge-find-def)
;  (require 'yasnippet)
;  (yas-global-mode 1)
;  (global-lsp-bridge-mode))
;; End Lsp-bridge

;; Tramp
(setq tramp-default-method "sshx")
(setq projectile-mode-line "Projectile")
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
;; End of Tramp

;; org-mode 插入粘贴板上的图片
  (defun org-insert-image ()
    "Insert an image from clipboard into a folder named after the current Org file."
    (interactive)
    (let* ((org-file-buffer (buffer-name))
           (org-file-name (file-name-base org-file-buffer)) ; 获取当前Org文件的文件名
           (path (concat default-directory org-file-name "/")) ; 创建与Org文件同名的文件夹
           (image-file (concat
                        path
                        (format-time-string "%Y%m%d_%H%M%S.png"))))
      (if (not (file-exists-p path))
          (make-directory path t)) ; 如果文件夹不存在，创建它
      (do-applescript (concat
                       "set the_path to \"" image-file "\" \n"
                       "set png_data to the clipboard as «class PNGf» \n"
                       "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                       "write png_data to the_file \n"
                       "close access the_file"))
      (org-insert-link nil
                       (concat "file:" image-file)
                       "")
      (message image-file))
    (org-display-inline-images))

