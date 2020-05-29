;;; init-pdf.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-defs))

;; PDFToolsPac
(use-package pdf-tools-install
  :ensure pdf-tools
  :if (and *sys/gui* (not *sys/win32*) (not *eaf-env*))
  :mode "\\.pdf\\'"
  :commands (pdf-loader-install)
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install))
;; -PDFToolsPac

(provide 'init-pdf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pdf.el ends here