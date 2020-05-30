;;; init-global.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `global-set-key'
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-s") #'save-all-buffers)
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)
(global-set-key (kbd "M-W =") (lambda () (interactive) (resize-window t 5)))
(global-set-key (kbd "M-W M-+") (lambda () (interactive) (resize-window t 5)))
(global-set-key (kbd "M-W -") (lambda () (interactive) (resize-window t -5)))
(global-set-key (kbd "M-W M-_") (lambda () (interactive) (resize-window t -5)))
(global-set-key (kbd "M-H =") (lambda () (interactive) (resize-window nil 5)))
(global-set-key (kbd "M-H M-+") (lambda () (interactive) (resize-window nil 5)))
(global-set-key (kbd "M-H -") (lambda () (interactive) (resize-window nil -5)))
(global-set-key (kbd "M-H M-_") (lambda () (interactive) (resize-window nil -5)))
(global-set-key (kbd "C-z e") #'edit-configs)

;; -END

;;----------------------------------------------------------------------------
;; `encoding'
;;----------------------------------------------------------------------------
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -END

;;----------------------------------------------------------------------------
;; `hook'
;;----------------------------------------------------------------------------
(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)
(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'before-save-hook #'save-and-update-includes)
(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)
(add-hook 'post-command-hook #'smart-electric-indent-mode)
;; -END

;;----------------------------------------------------------------------------
;; `setq'
;;----------------------------------------------------------------------------

(setq user-full-name "ZhiCheng Lee")
(setq user-mail-address "gccll.love@gmail.com")
(setq x-alt-keysym 'meta)
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1)
(setq ad-redefinition-action 'accept)
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq hscroll-step 1)
(setq hscroll-margin 1)

(setq-default create-lockfiles nil)
(setq-default history-length 500)
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 2)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 2)
;; -END

;;----------------------------------------------------------------------------
;; `feature-mode'
;;----------------------------------------------------------------------------
(delete-selection-mode 1)
(save-place-mode 1)
(toggle-scroll-bar -1)
(fset 'yes-or-no-p 'y-or-n-p)
(toggle-frame-maximized)
(load custom-file 'noerror)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)
;; -END

;;----------------------------------------------------------------------------
;; `cond'
;;----------------------------------------------------------------------------
;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))
;; -END

;;----------------------------------------------------------------------------
;; `add-to-list'
;;----------------------------------------------------------------------------
;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -END

(provide 'init-global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global.el ends here
