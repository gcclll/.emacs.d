;;; init-global.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-def))

;;----------------------------------------------------------------------------
;; `global-set-key'
;;----------------------------------------------------------------------------
;; (global-set-key (kbd "C-+") #'text-scale-increase)
;; (global-set-key (kbd "C--") #'text-scale-decrease)
;; -END

;;----------------------------------------------------------------------------
;; `hook'
;;----------------------------------------------------------------------------
;; (add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)
;; (add-hook 'after-init-hook #'global-emojify-mode)
;; (add-hook 'before-save-hook #'save-and-update-includes)
;; (add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)
;; (add-hook 'post-command-hook #'smart-electric-indent-mode)
;; (add-hook 'prog-mode-hook 'add-pretty-lambda)
;; (add-hook 'org-mode-hook 'add-pretty-lambda)
;; (if (version< emacs-version "26")
    ;; (global-linum-mode)
;;   (add-hook 'text-mode-hook #'display-line-numbers-mode)
;;   (add-hook 'prog-mode-hook #'display-line-numbers-mode))
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; (add-hook 'prog-mode-hook #'hs-minor-mode)
;; -END

;;----------------------------------------------------------------------------
;; `setq'
;;----------------------------------------------------------------------------

(setq warning-minimum-level :emergency)
;; (setq user-full-name "ZhiCheng Lee")
(setq user-blog-url "https://blog.ii6g.com")
;; (setq user-mail-address "gccll.love@gmail.com")
;; (setq x-alt-keysym 'meta)
;; (setq ring-bell-function 'ignore)
;; (setq echo-keystrokes 0.1)
;; (setq ad-redefinition-action 'accept)
;; (setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
;; Add a newline automatically at the end of the file upon save.
;; (setq require-final-newline t)
;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101)
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)
;; (setq inhibit-startup-screen t)
;; (setq initial-major-mode 'text-mode)
(setq initial-scratch-message "落叶相依浑似醉，潦倒何妨；悠悠岁月谁高歌，绝胜柳狂。\n")

;; (setq-default create-lockfiles nil)
;; (setq-default history-length 500)
;; (setq-default compilation-always-kill t) ; kill compilation process before starting another
;; (setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
;; (setq-default compilation-scroll-output t)
;; (setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; (setq-default indent-tabs-mode nil)
;; (setq-default indent-line-function 'insert-tab)
;; (setq-default tab-width 2)
;; (setq-default c-basic-offset 4)
;; (setq-default js-switch-indent-offset 2)
(setq-default frame-title-format '("李志诚❤范婷婷 - " user-login-name "@" system-name " - %b"))
;; -END

;;----------------------------------------------------------------------------
;; `feature-mode'
;;----------------------------------------------------------------------------
;; (global-prettify-symbols-mode 1)
;; (column-number-mode 1)
;; (delete-selection-mode 1)
;; (display-time-mode 1)
;; (display-battery-mode 1)
;; (save-place-mode 1)
;; (toggle-scroll-bar -1)
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (load custom-file 'noerror)
;; (c-set-offset 'comment-intro 0)
;; (c-set-offset 'innamespace 0)
;; (c-set-offset 'case-label '+)
;; (c-set-offset 'access-label 0)
;; (c-set-offset (quote cpp-macro) 0 nil)
;; -END

;;----------------------------------------------------------------------------
;; `cond'
;;----------------------------------------------------------------------------
;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
;; (when (fboundp 'global-so-long-mode)
;;   (global-so-long-mode))
;; -END

;;----------------------------------------------------------------------------
;; `add-to-list'
;;----------------------------------------------------------------------------
;; Default .args, .in, .out files to text-mode
;; (add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
;; (add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
;; (add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -END

;;----------------------------------------------------------------------------
;; `remap'
;;----------------------------------------------------------------------------
;; TODOs
;; -END

(provide 'init-global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global.el ends here