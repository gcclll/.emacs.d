;;; init-search.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-global)
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `ivy'
;;----------------------------------------------------------------------------
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-z s" . counsel-rg)
   ("C-z b" . counsel-buffer-or-recentf)
   ("C-z C-b" . counsel-ibuffer)
   (:map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-RET" . ivy-immediate-done))
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))
;; -END

;;----------------------------------------------------------------------------
;; `color-rg'
;;----------------------------------------------------------------------------
(use-package color-rg
  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
  :if *rg*
  :bind ("C-M-s" . color-rg-search-input))
;; -END

;;----------------------------------------------------------------------------
;; `anzu'
;;----------------------------------------------------------------------------
(use-package anzu
  :init
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))
;; -END


;;----------------------------------------------------------------------------
;; `fzf'
;;----------------------------------------------------------------------------
(use-package fzf
  :commands fzf/start
  :bind (("C-c f" . fzf)
         ("C-c D" . gcl/lcd)))
;; -END


;;----------------------------------------------------------------------------
;; `find-file-in-project'
;;----------------------------------------------------------------------------
(use-package find-file-in-project
  :if *find*)
;; -END

;;----------------------------------------------------------------------------
;; `rg'
;;----------------------------------------------------------------------------
;; https://blog.burntsushi.net/ripgrep/
(use-package rg
  :init
  (rg-enable-default-bindings)
  :config
  (setq rg-executable "/opt/local/bin/rg"))
;; -END

;;----------------------------------------------------------------------------
;; `snails'
;;----------------------------------------------------------------------------
(use-package snails
  :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
  :if *sys/gui*
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :init
  (use-package exec-path-from-shell :if (featurep 'cocoa) :defer t)
  :config
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind))))
;; -END


(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
