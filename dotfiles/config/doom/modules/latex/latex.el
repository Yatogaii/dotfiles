(require 'tex)
;; 配合 AUCTex
(setq TeX-PDF-mode t)
(add-to-list 'TeX-command-list
             '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX"))
(setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
(setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
(setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索


;; 用pdf-tools 打开pdf
(setq Tex-view-program-selection '((output-pdf "PDF Tools")))
;; 自动刷新
(add-hook 'Tex-after-compilation-finished-functions 
          #'Tex-revert-document-buffer)
