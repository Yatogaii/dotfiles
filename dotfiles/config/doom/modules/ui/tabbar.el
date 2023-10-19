;; 全部的buffer都分一组，否则这个修改是没任何意思的

(setq tabbar-buffer-groups-function
	  (lambda () (list "All Buffers")))

;; 去掉emacs自带的几个buffer

(setq tabbar-buffer-list-function
	  (lambda ()
	(remove-if
	 (lambda(buffer)
	   (find (aref (buffer-name
			buffer) 0) " *"))
	 (buffer-list))))

;; 切换到第N个buffer，1为第一个，负数表示从后数，注意0会出错，这里就不处理了
(defun switch-tabbar (num)
  (let* ((tabs (tabbar-tabs (tabbar-get-tabset "All Buffers")))
	 (tab (nth (if (> num 0) (- num 1) (+ (length tabs) num)) tabs)))
	(if tab (switch-to-buffer (car tab)))))

;; 不说废话，绑热键
(global-set-key [(meta 1)] (lambda () (interactive) (switch-tabbar 1)))
(global-set-key [(meta 2)] (lambda () (interactive) (switch-tabbar 2)))
(global-set-key [(meta 3)] (lambda () (interactive) (switch-tabbar 3)))
(global-set-key [(meta 4)] (lambda () (interactive) (switch-tabbar 4)))
(global-set-key [(meta 5)] (lambda () (interactive) (switch-tabbar 5)))
(global-set-key [(meta 6)] (lambda () (interactive) (switch-tabbar 6)))
(global-set-key [(meta 7)] (lambda () (interactive) (switch-tabbar 7)))
(global-set-key [(meta 8)] (lambda () (interactive) (switch-tabbar 8)))
(global-set-key [(meta 9)] (lambda () (interactive) (switch-tabbar 9)))
