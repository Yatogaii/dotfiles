(defun toggle-right-window ()
  "Toggle the right window display while preserving window layout.
   When there are two windows, close the right one and move cursor to the left if it's in the right.
   When there is only one window, create a new one on the right and move cursor to it."
  (interactive)
  (let ((current-window (selected-window))
        (window-count (length (window-list))))
    ;; 检查窗口数量
    (cond
     ((= window-count 2) ;; 如果有两个窗口
      (let ((right-window (window-in-direction 'right)))
        (when right-window ;; 检查右侧窗口是否存在
          (delete-window right-window) ;; 关闭右侧窗口
          (when (equal current-window right-window) ;; 如果光标在右侧窗口
            (select-window (window-in-direction 'left)))))) ;; 选择左侧窗口
     ((= window-count 1) ;; 如果只有一个窗口
      (select-window (split-window-right)) ;; 分割窗口并选择新窗口
      (switch-to-buffer (other-buffer))) ;; 打开另一个缓冲区
     (t ;; 如果窗口数量不是1或2，不执行任何操作
      nil))))


(map! :leader
      :desc "Toggle right window"
      "w c" #'toggle-right-window)
