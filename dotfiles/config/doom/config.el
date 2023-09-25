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
(setq doom-theme 'doom-bluloco-light)

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
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/OneDrive/Orgs/Roam/")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (setq org-roam-capture-templates
      '(("a" "Academic")
        ("av" "Virtualization" plain
         "%?"
         :if-new (file+head "Academic/Virtualization/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("ab" "Basics" plain "%?"
         :if-new
         (file+head "Academic/Basics/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("ap" "Papers" plain "%?"
         :if-new
         (file+head "Academic/Papers/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed nil) ; Papers 不被 Org Roam 管理
        ("p" "Person")
        ("ps" "School" plain "%?"
         :if-new (file+head "Person/School/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("pl" "Life" plain "%?"
         :if-new (file+head "Person/Life/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("pr" "Reading" plain "%?"
         :if-new (file+head "Person/Reading/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("w" "Work")
        ("wn" "Notes" plain "%?"
         :if-new (file+head "Work/Notes/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed nil) ; Notes 不被 Org Roam 管理
        ("wt" "Todo" plain "%?"
         :if-new (file+head "Work/Todo/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed nil) ; Todo 不被 Org Roam 管理
        ("r" "Reference")
        ("rt" "Tech" plain "%?"
         :if-new (file+head "Reference/Tech/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("rl" "Life" plain "%?"
         :if-new (file+head "Reference/Life/${slug}.org" "#+title: ${title}\n")
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


  ; START EAF
  ; EAF :: https://github.com/emacs-eaf/emacs-application-framework/issues/909
;  (add-to-list 'load-path "~/.config/emacs/.local/straight/repos/emacs-application-framework/")
;  (use-package! eaf
;    :commands 
;      (eaf-open-browser
;       eaf-open
;       find-file
;       eaf-interleave-open-notes-file
;       eaf-interleave-add-note
;       eaf-interleave-mode
;       eaf-interleave-sync-current-note)
;    :custom
;    (eaf-find-alternate-file-in-dired t)
;    :diminish eaf-mode
;    :bind (:map eaf-interleave-mode-map
;           ("M-." . 'eaf-interleave-sync-current-note)
;           ("M-p" . 'eaf-interleave-sync-previous-note)
;           ("M-n" . 'eaf-interleave-sync-next-note)
;           :map eaf-interleave-app-mode-map
;           ("C-c M-i" . 'eaf-interleave-add-note)
;           ("C-c M-o" . 'eaf-interleave-open-notes-file)
;           ("C-c M-q" . 'eaf-interleave-quit))
;    :config
;      (require 'eaf-rss-reader)
;      (require 'eaf-terminal)
;      (require 'eaf-image-viewer)
;      (require 'eaf-pdf-viewer)
;      (require 'eaf-markdown-previewer)
;      (require 'eaf-file-browser)
;      (require 'eaf-file-manager)
;      (require 'eaf-video-player)
;      (require 'eaf-git)
;      (require 'eaf-system-monitor)
;      (require 'eaf-interleave)
;      ;; EAF interleave config
;      (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
;      (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
;      (add-hook 'org-mode-hook 'eaf-interleave-mode)
;      (setq eaf-interleave-org-notes-dir-list '("~/Onedrive/Orgs/Interleaves"))
;      (setq eaf-interleave-split-direction 'vertical)
;      (setq eaf-interleave-disable-narrowing t)
;      (setq eaf-interleave-split-lines 20))
;
;; END OF EAF


