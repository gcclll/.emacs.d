
;; --- js2
(defun my/setup-js-mode ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq tab-width 2)
  ;; 由于 lsp 已经提供了 diagnose 功能，故关闭 js2 自带的错误检查，防止干扰。
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors nil)
  )

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'my/setup-js-mode)
  (add-hook 'json-mode-hook #'my/setup-js-mode))

;; --- ts
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . my/setup-js-mode)))

;; --- json
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; --- css
(use-package css-mode)
(use-package scss-mode)

;; --- emmet
(use-package emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  )

;; --- web
(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
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
  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "vue"))
  )

(provide 'init-web)
