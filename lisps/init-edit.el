;;; init-edit.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-global))

;;----------------------------------------------------------------------------
;; `pkgs'
;;----------------------------------------------------------------------------
(use-package expand-region
 :bind ("C-=" . er/expand-region))

(use-package smart-region
 :hook (after-init . smart-region-on))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(use-package iedit
  :diminish)

(use-package multiple-cursors)
;; -END

;; AwesomePairPac
;;----------------------------------------------------------------------------
;; `pair'
;;----------------------------------------------------------------------------
(use-package awesome-pair
  :load-path (lambda () (expand-file-name "site-elisp/awesome-pair" user-emacs-directory))
  :bind
  (:map prog-mode-map
        (("M-D" . awesome-pair-kill)
         ("SPC" . awesome-pair-space)
         ("=" . awesome-pair-equal)
         ("M-F" . awesome-pair-jump-right)
         ("M-B" . awesome-pair-jump-left)))
  :hook (prog-mode . awesome-pair-mode))

(use-package conf-mode
  :ensure nil
  :bind
  (:map conf-mode-map
        (("M-D" . awesome-pair-kill)
         ("SPC" . awesome-pair-space)
         ("=" . awesome-pair-equal)
         ("M-F" . awesome-pair-jump-right)
         ("M-B" . awesome-pair-jump-left))))
;; -END


;;----------------------------------------------------------------------------
;; `delete-block'
;;----------------------------------------------------------------------------
(use-package delete-block
  :load-path (lambda () (expand-file-name "site-elisp/delete-block" user-emacs-directory))
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))
;; -END

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
