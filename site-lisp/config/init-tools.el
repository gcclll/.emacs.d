(use-package nvm
  :defer t)

;; --- crux
(use-package crux)

;; --- fanyi
  (use-package fanyi
    :config
    (custom-set-variables
     '(fanyi-providers '(fanyi-haici-provider
			 fanyi-youdao-thesaurus-provider
			 fanyi-etymon-provider
			 fanyi-longman-provider
			 ;; fanyi-libre-provider
			 )))

    ;; 还要自动选择翻译内容 buffer
    (setq fanyi-auto-select nil))

;; --- youdao
(use-package youdao-dictionary)

;; --- link-hint
(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;; --- math-at-point
;; Test: Result 6.23+(3.789/(5-4)) + 6.4*(2 - (5+3) *736.83 ) /2000
(require 'math-at-point)

;; --- uuidgen
(use-package uuidgen)

;; --- dash-at-point
(use-package dash-at-point)

;; --- app-launcher
;; (require 'app-launcher)

;; --- devdocs
(use-package devdocs)

;; --- httprepl
(use-package httprepl)

;; --- dockfile
(use-package dockerfile-mode)

;; --- restclient
(use-package restclient)

(provide 'init-tools)
