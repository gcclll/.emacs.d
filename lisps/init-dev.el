;;; init-dev.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `dev-tools'
;;----------------------------------------------------------------------------
(use-package dumb-jump
  :custom (dumb-jump-selector 'ivy))

(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(use-package evil-nerd-commenter
  :bind
  (("C-c M-;" . c-toggle-comment-style)
   ("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package ein
  :disabled
  :defer t)

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (python-mode . eglot-ensure)))
;; -END

;;----------------------------------------------------------------------------
;; `tiny-mode'
;;----------------------------------------------------------------------------
(use-package yaml-mode
  :commands (yaml-mode)
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))


;;----------------------------------------------------------------------------
;; `dev languages'
;;----------------------------------------------------------------------------
;; LSPJavaPac
(use-package lsp-java
  :after lsp-mode
  :if *mvn*
  :init
  (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))

(use-package ess
  :defer t
  :commands R
  :config
  (load "ess-autoloads"))

(use-package docker)
(use-package dockerfile-mode :defer t)
(use-package groovy-mode :defer t)

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
