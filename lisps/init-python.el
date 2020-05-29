;;; init-python.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-flycheck)
  (require 'init-defs))

;; PythonConfig
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
;; -PythonConfig

;; LSPPythonPac
(use-package lsp-python-ms
  :after lsp-mode python
  :if *python*
  :custom
  (lsp-python-executable-cmd "python3"))
;; -LSPPythonPac

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here