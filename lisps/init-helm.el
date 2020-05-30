;;; init-helm.el --- helm search
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `helm-swoop'
;;----------------------------------------------------------------------------

(use-package helm)

(use-package helm-grep
  :defer t
  :config
  (bind-keys :map helm-grep-mode-map
             ("RET" . helm-grep-mode-jump-other-window)
             ("n" . helm-grep-mode-jump-other-window-forward)
             ("p" . helm-grep-mode-jump-other-window-backward)))

(use-package helm-ag
  :defer t)

(use-package helm-swoop
  :bind
  (("M-o" . helm-swoop)
   ("M-O" . helm-swoop-back-to-last-point)
   ("C-c M-o" . helm-multi-swoop)
   ;; ("C-c M-O" . helm-multi-swoop-all)
   )
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  (bind-keys :map isearch-mode-map
             ("M-o" . helm-swoop-from-isearch))
  (bind-keys :map helm-swoop-map
             ("M-o" . helm-multi-swoop-all-from-helm-swoop)
             ;; ("M-i" . helm-swoop-from-evil-search)
             )
  )
;; -END


(provide 'init-helm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
