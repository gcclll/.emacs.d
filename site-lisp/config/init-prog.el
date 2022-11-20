
;; --- yaml
(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . yaml-imenu-enable)))
(use-package yaml-imenu
  :after yaml-mode)

;; --- python
(use-package python-mode)

;; --- xml
(require 'nxml-mode)

;; --- golang
(use-package go-mode)

;; --- php
(use-package php-mode)

;; --- sql
(require 'sql)
(require 'sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; --- lua
(use-package lua-mode)

;; --- rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

;; --- format-all
(use-package format-all)

;; --- flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; --- pkg-info
(use-package pkg-info)

(provide 'init-prog)
