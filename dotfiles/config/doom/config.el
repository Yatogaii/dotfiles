;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; CN mirrors
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org-cn". "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
  (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'normal :width 'normal)
        doom-unicode-font (font-spec :family "LXGW WenKai" :size 16))
;  (setq doom-font (font-spec :family "LXGW WenKai Mono" :size 16 :weight 'normal :width 'normal))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'modus-vivendi-tritanopia)
(setq doom-theme 'doom-tomorrow-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; org and org-roam directory
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/Orgs/")

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "jethro/org-capture-slipbox" "<tab>" #'jethro/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-id-get-create" "z" #'org-id-get-create)
  :config
  (setq org-roam-directory (file-truename "~/OneDrive/Orgs/Roam/")
	org-roam-database-connector 'sqlite-builtin
	org-roam-db-gc-threshold most-positive-fixnum)
  (with-eval-after-load 'org-id
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("n" "no-org-roam" plain "%?"
         :if-new
         (file+head "../no-roam/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

  ;; Org-Roam node display setting
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node))))
  (setq org-roam-node-display-template (concat "${type:15} ${hierarchy:*}" (propertize "${tags:10}" 'face 'org-tag)))

  (defun jethro/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (require 'citar)
  (defun jethro/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
                                        :ROAM_REFS: [cite:@${citekey}]
                                        :END:
                                        #+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))
;; End of org-roam

;; Start of Citar

(use-package! citar
  :custom
  (citar-bibliography '("~/OneDrive/Orgs/虚拟化安全.bib")))

;; End of citar

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

; auto indented
(setq org-startup-indented t)

; map jk to esc
(setq-default evil-escape-key-sequence "jk")

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
    (rime-librime-root "~/.config/emacs/librime/dist")
    (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@30/30.0.50/include")
    (default-input-method "rime")
    :bind
    (:map rime-active-mode-map
    ;("<tab>" . 'rime-inline-ascii)))
    ))
  (setq rime-user-data-dir "~/Library/Rime")
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
(use-package! corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
;  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
;  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

;; better corfu experience
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

;; CORFU End

;; Optionally use the `orderless' completion style.
(use-package! orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Start LSP
;; 配置 lsp-mode
;(load! "modules/completions/eglot.el")
;; End LSP

;; tab bar
;(load! "modules/ui/tabbar.el")

;; org-journal
(load! "modules/org/org-journal.el")

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

