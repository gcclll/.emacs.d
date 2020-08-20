;;; init-evil.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :init (setq evil-want-keybinding nil)
  :config
  (setq evil-default-state 'normal)
  ;; 不希望使用 evil mode 的内容放在这
  ;; (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  ;; (evil-set-initial-state 'dashboard-mode 'emacs)
  ;; evil ex command `:W' to save all buffers
  (evil-ex-define-cmd "W" 'evil-write-all)

  ;; evil normal state keybinds
  (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
  ;; (define-key evil-motion-state-map (kbd "s-k") 'crux-kill-line-backwards)
  (define-key evil-normal-state-map "Y" (kbd "y$"))
	(define-key evil-normal-state-map (kbd "{") 'evil-previous-open-brace)
	(define-key evil-normal-state-map (kbd "}") 'evil-next-close-brace)
	(define-key evil-normal-state-map (kbd "(") 'evil-previous-open-paren)
	(define-key evil-normal-state-map (kbd ")") 'evil-next-close-paren)
	(define-key evil-normal-state-map (kbd "%") 'vr/query-replace)
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq evil-shift-width 2)))
  )

;; gcc comments out a line
;; gc comments out the target of motion
;; gcap comments out a paragraph
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :diminish evil-matchit-mode
  :config (global-evil-matchit-mode t))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :hook (after-init . global-evil-mc-mode)
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode)
  :init (global-evil-mc-mode 1))

(use-package evil-mc-extras
  :after evil-mc
  :diminish evil-mc-extras-mode)

(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
