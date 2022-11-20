
;; --- yaml
(require 'yaml-mode)
(require 'yaml-imenu)
(add-hook 'yaml-mode-hook 'yaml-imenu-enable)

;; --- python
(require 'python-mode)

;; --- xml
(require 'nxml-mode)

;; --- php
(require 'php-mode)

;; --- sql
(require 'sql)
(require 'sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; --- lua
(require 'lua-mode)

;; --- rust
(require 'rust-mode)
(setq rust-format-on-save t)
(require 'cargo)

;; --- format-all
(require 'format-all)

;; --- flycheck
(require 'flycheck)
(global-flycheck-mode)

;; --- pkg-info
(require 'pkg-info)

(provide 'init-prog)
