;;; init-dev.el --- -*- lexical-binding: t -*-
;;; Code:

;; DumbJump
(use-package dumb-jump
  :custom (dumb-jump-selector 'ivy))
;; -DumbJump

;; QuickrunPac
(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))
;; -QuickrunPac

;; FormatAllPac
(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))
;; -FormatAllPac

;; EvilNerdCommenPac
(use-package evil-nerd-commenter
  :bind
  (("C-c M-;" . c-toggle-comment-style)
   ("M-;" . evilnc-comment-or-uncomment-lines)))
;; -EvilNerdCommenPac

;; EINPac
(use-package ein
  :disabled
  :defer t)
;; -EINPac

;; Eglot
(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))
;; -Eglot

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
;; -LSPJavaPac

;; ESSPac
(use-package ess
  :defer t
  :commands R
  :config
  (load "ess-autoloads"))
;; -ESSPac

;; DockerPac
(use-package docker)
;; -DockerPac

;; DockerfilePac
(use-package dockerfile-mode :defer t)
;; -DockerfilePac

;; GroovyPac
(use-package groovy-mode :defer t)
;; -GroovyPac

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
