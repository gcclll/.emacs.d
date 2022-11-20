

;; --- crux
(require 'crux)

;; --- fanyi
(require 'fanyi)
(custom-set-variables
 '(fanyi-providers '(
		     ;; 海词
		     fanyi-haici-provider
		     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
		     ;; Etymonline
                     fanyi-etymon-provider
                     fanyi-longman-provider
		     ;; LibreTranslate
                     ;; fanyi-libre-provider
		     )))
;; 不要自动选择翻译内容 buffer
(setq fanyi-auto-select nil)

;; --- youdao
(require 'youdao-dictionary)

;; --- link-hint
(require 'link-hint)
(general-define-key
 "C-c l o" 'link-hint-open-link
 "C-c l c" 'link-hint-copy-link)

;; --- math-at-point
;; Test: Result 6.23+(3.789/(5-4)) + 6.4*(2 - (5+3) *736.83 ) /2000
(require 'math-at-point)

;; --- uuidgen
(require 'uuidgen)

;; --- dash-at-point
(require 'dash-at-point)

;; --- app-launcher
(require 'app-launcher)

;; --- devdocs
(require 'devdocs)

;; --- httprepl
(require 'httprepl)

;; --- dockfile
(require 'dockerfile-mode)

;; --- restclient
(require 'restclient)

(provide 'init-tools)
