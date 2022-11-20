
;; --- expand-region
(use-package expand-region)

;; --- smartparens
(use-package smartparens
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
	:map smartparens-strict-mode-map
        ("C-M-<backspace>" . sp-backward-unwrap-sexp)
        ("C-M-d" . sp-unwrap-sexp))
  :hook
  ((prog-mode . smartparens-mode)
   ;; (smartparens-mode . smartparens-strict-mode)
   )
  :config (require 'smartparens-config))

;; --- autorevert
(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; --- hungry-delete
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; --- symbol-overlay
(use-package symbol-overlay)

;; --- move-text
(use-package move-text)

;; --- editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; --- toggle-quotes-plus
(require 'toggle-quotes-plus)
(setq toggle-quotes-plus-chars '("\""
                                 "'"
                                 "`"))

;; --- duplicate-line
(require 'duplicate-line)

(provide 'init-edit)
