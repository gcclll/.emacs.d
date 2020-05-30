;;; init-ediff.el --- ediff
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `ediff'
;;----------------------------------------------------------------------------
(use-package ediff
  :config
  (setq ediff-merge-split-window-function 'split-window-vertically
        ediff-window-setup-function 'ediff-setup-windows-plain)
  (add-to-list 'evil-emacs-state-modes 'ediff-mode))
;; -END


(provide 'init-ediff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ediff.el ends here
