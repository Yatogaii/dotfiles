;;; package -- Summary
;;; Commentary:
;;; org mode 相关的配置文件
;;;
;;; Code:
(setq yt-org-base-dir "~/OneDrive/Orgs")
(setq org-directory yt-org-base-dir)
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
  (setq org-roam-directory (file-truename yt-org-base-dir)
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
         (file+head "no-roam/${title}.org" "#+title: ${title}\n")
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


(defun yt/outline-left ()
  (interactive)
  (hide-subtree)
  (outline-up-heading 1)
  (hide-subtree)
  (outline-show-children)
  (outline-show-entry))

(defun yt/outline-up ()
  (interactive)
  (hide-subtree)
  (outline-backward-same-level 1)
  (outline-show-children)
  (outline-show-entry))

(defun yt/outline-down ()
  (interactive)
  (hide-subtree)
  (outline-forward-same-level 1)
  (outline-show-children)
  (outline-show-entry))

(defun yt/outline-right ()
  (interactive)
  (if (outline-has-subheading-p)
      (progn  (outline-next-heading)
              (outline-show-children)
              (outline-show-entry))
      ))

(use-package! org
  :init
  (map! :leader
        :desc "yt/outline-left" "h" #'yt/outline-left
        :desc "yt/outline-down" "j" #'yt/outline-down
        :desc "yt/outline-up" "k" #'yt/outline-up
        :desc "yt/outline-right" "l" #'yt/outline-right
	:desc "counsel-org-link" "n I" #'counsel-org-link))

(defun yt/my-org-narrow-to-subtree ()
  (interactive)
  (org-narrow-to-subtree)
  (org-indent-mode -1))

(defun yt/my-org-widen ()
  (interactive)
  (widen)
  (org-indent-mode 1))

(define-key org-mode-map (kbd "C-x n s") 'yt/my-org-narrow-to-subtree)
(define-key org-mode-map (kbd "C-x n w") 'yt/my-org-widen)

; counsel-org-link no full path
(after! counsel
  (setq counsel-outline-display-style 'title))
(define-key org-mode-map (kbd "C-x n s") 'yt/my-org-narrow-to-subtree)
