;;; init-webdev.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `web-mode'
;;----------------------------------------------------------------------------
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'")
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-current-element-highlight t)
  (emmet-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
;; -END


;;----------------------------------------------------------------------------
;; `js2-mode'
;;----------------------------------------------------------------------------
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-strict-missing-semi-warning nil
                js-switch-indent-offset 2
                js-indent-level 2
                js2-basic-offset 2)
  (use-package add-node-modules-path
    :after typescript-mode js2-mode
    :config
    (add-hook 'typescript-mode-hook 'add-node-modules-path)
    (add-hook 'js2-mode-hook 'add-node-modules-path)))
;; -END

;;----------------------------------------------------------------------------
;; `typescript-mode'
;;----------------------------------------------------------------------------
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode)
  :config
  (setq-default typescript-indent-level 2))
;; -END

;;----------------------------------------------------------------------------
;; `css/scss/less/stylus'
;;----------------------------------------------------------------------------
(use-package rainbow-mode)
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  (setq scss-compile-at-save nil))

(use-package sass-mode
  :disabled t
  :ensure t
  :mode "\\.scss\\'")
;; -END

;;----------------------------------------------------------------------------
;; `emmet'
;;----------------------------------------------------------------------------
(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :config
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'rjsx-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'emmet-mode-hook (lambda()
                              (setq emmet-indent-after-insert t))))

;; rjsx-mode use className
(use-package mode-local
  :ensure t
  :config
  (setq-mode-local rjsx-mode emmet-expand-jsx-className? t)
  (setq-mode-local web-mode emmet-expand-jsx-className? nil))
;; -END

;; InstantRenameTagPac
(use-package instant-rename-tag
  :load-path (lambda () (expand-file-name "site-elisp/instant-rename-tag" user-emacs-directory)))
;; -InstantRenameTagPac

;; JsonPac
(use-package json-mode
  :mode "\\.json\\'")
;; -JsonPac

;;----------------------------------------------------------------------------
;; `prettier-js'
;;----------------------------------------------------------------------------
(use-package prettier-js
  :diminish prettier-js-mode
  :commands (prettier-js-mode prettier)
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  ;; (add-hook 'vue-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-target-mode "js2-mode")
  (setq prettier-js-args '(
                           "--trailing-comma" "none"
                           "--print-width" "80"
                           "--tab-width" "2"
                           "--single-quote" "true"
                           "--no-semi"
                           ))
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode
                                '("\\.jsx?\\'" . prettier-js-mode)))))
;;----------------------------------------------------------------------------
;; `rjsx-mode'
;;----------------------------------------------------------------------------
(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("components?\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("containers?\\/.*\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'setup-tide-mode))

(use-package react-snippets
  :ensure t)
;; -END

;;----------------------------------------------------------------------------
;; `tide'
;;----------------------------------------------------------------------------
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
;; -END

;;----------------------------------------------------------------------------
;; `python'
;;----------------------------------------------------------------------------
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package lsp-python-ms
    :init (require 'lsp-python-ms))
  (use-package py-isort)
  (use-package python-black)
  ;; (use-package py-yapf)
  :hook (
         (python-mode . lsp-deferred)
         (python-mode . py-isort-enable-on-save)
         (python-mode . python-black-on-save-mode)
         )
  )
;; -END

(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
