
;; --- expand-region
(require 'expand-region)

;; --- smartparens
(require 'smartparens)
(general-def smartparens-mode-map
  "C-M-f" 'sp-forward-sexp
  "C-M-b" 'sp-backward-sexp
  "C-M-k" 'sp-kill-sexp
  "C-M-w" 'sp-copy-sexp
  )

(add-hook 'prog-mode-hook 'smartparens-mode)
;; (add-hook 'smartparens-mode-hook #'smartparens-strict-mode)

(require 'smartparens-config)

;; --- autorevert
(require 'autorevert)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; --- hungry-delete
(require 'hungry-delete)
(add-hook 'after-init-hook 'global-hungry-delete-mode)
(setq-default hungry-delete-chars-to-skip " \t\f\v")

;; --- symbol-overlay
(require 'symbol-overlay)

;; --- move-text
(require 'move-text)

;; --- editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; --- toggle-quotes-plus
(require 'toggle-quotes-plus)
(setq toggle-quotes-plus-chars '("\""
                                 "'"
                                 "`"))

(provide 'init-edit)
