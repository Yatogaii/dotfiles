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


(evil-define-key 'normal org-mode-map
  (kbd "<SPC>-h") 'yt/outline-left
  (kbd "<SPC>-j") 'yt/outline-down
  (kbd "<SPC>-k") 'yt/outline-up
  (kbd "<SPC>-l") 'yt/outline-right)

