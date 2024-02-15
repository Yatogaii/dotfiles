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
        :desc "yt/outline-right" "l" #'yt/outline-right))

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
