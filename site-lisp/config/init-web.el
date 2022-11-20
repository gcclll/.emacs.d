
;; --- js2
(require 'js2-mode)
;; Use js2-mode for Node scripts
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
;; Don't use built-in syntax checking
(setq js2-mode-show-strict-warnings nil)
(setq js2-mode-show-parse-errors nil)
(setq evil-shift-width js-indent-level)

;; --- ts
(require 'typescript-mode)

;; --- json
(require 'json-mode)

;; --- css
(require 'css-mode)
(require 'scss-mode)

;; --- emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'js2-mode-hook 'emmet-mode)
;; indent 2 spaces.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; --- web
(require 'web-mode)
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-style-padding 0
      web-mode-script-padding 0
      web-mode-enable-auto-closing t
      web-mode-enable-auto-opening t
      web-mode-enable-auto-pairing nil
      web-mode-enable-auto-indentation t
      web-mode-tag-auto-close-style 1
      web-mode-enable-current-element-highlight t)

;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
(dolist (alist web-mode-engines-auto-pairs)
  (setcdr alist
	  (cl-loop for pair in (cdr alist)
		   unless (string-match-p "^[a-z-]" (cdr pair))
		   collect (cons (car pair)
				 (string-trim-right (cdr pair)
						    "\\(?:>\\|]\\|}\\)+\\'")))))

(provide 'init-web)
