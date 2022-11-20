
(require 'package)

(setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
			                   ("melpa" . "http://1.15.88.122/melpa/")))

(package-initialize)


;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Diminish functionality
(use-package diminish)

(provide 'init-package)
