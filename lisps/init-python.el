;;; init-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-flycheck)
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `python-mode'
;;----------------------------------------------------------------------------
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
;; -END


;;----------------------------------------------------------------------------
;; `lsp-python-ms'
;;----------------------------------------------------------------------------
(use-package lsp-python-ms
  :after lsp-mode python
  :if *python*
  :custom
  (lsp-python-executable-cmd "python3"))
;; -END

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
