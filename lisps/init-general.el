;;; init-general.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-defs)
  (require 'init-global))

;;----------------------------------------------------------------------------
;; `SPC Leader'
;;----------------------------------------------------------------------------
(spcleader
    "SPC" 'counsel-M-x
    ;; <a>
    "a~" 'shell-here
    "a3" 'aweshell-dedicated-open
    "a#" 'aweshell-dedicated-close
    "a$" 'multi-term
    "ar" 'ranger
    ;; <c>
    ;; <b>
    "bb" 'counsel-switch-buffer
    "bd" 'kill-this-buffer
    "bi" 'ibuffer
    "bf" 'gcl/open-in-finder
    "bK" 'crux-kill-other-buffers
    "bo" 'crux-open-with
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "br" 'counsel-buffer-or-recentf
    "bs" 'save-buffer
    "bS" 'save-all-buffers
    "bm" 'buffer-menu
    ;; <c>
    "cp" 'crux-duplicate-current-line-or-region
    ;; <d>
    "do" 'open-dashboard
    "dd" 'dash-at-point
    ;; <f>
    "ff" 'counsel-find-file
    "fp" 'ffip
    "fr" 'crux-recentf-find-file
    "fR" 'crux-rename-file-and-buffer
    "fd" 'crux-delete-file-and-buffer
    ;; <g>
    "gg" 'magit-status
    ;; <h>
    "hr" 'hydra-rectangle/body
    "hm" 'hydra-multiple-cursors/body
    "ho" 'hydra-org-agenda/body
    ;; <i>
    "ie" 'emojify-insert-emoji
    ;; <o>
    "om" 'lsp-ui-imenu
    ;; <q>
    "qr" 'restart-emacs
    ;; <r>
    "rt" 'instant-rename-tag
    ;; <s>
    "ss" 'swiper
    "sS" 'swiper-all
    "sr" 'counsel-rg
    "si" 'swiper-isearch
    "sg" 'counsel-git-grep
    "sp" 'rg-project
    "sq" 'query-replace
    "sQ" 'query-replace-regexp
    ;; <w>
    "wv" 'split-window-horizontally
    "w-" 'split-window-vertically
    "wl" 'evil-window-right
    "wL" 'crux-transpose-windows
    "wH" 'crux-transpose-windows
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wd" 'delete-window
    "wm" 'delete-other-windows
    ;; <y>
    "yi" 'yas-insert-snippet
    "yn" 'yas-new-snippet
    )
;; -END


;;----------------------------------------------------------------------------
;; `Comma Leader'
;;----------------------------------------------------------------------------
(comaleader
  "go" 'dumb-jump-go-other-window
  "gj" 'dumb-jump-go
  "gb" 'dumb-jump-back
  "gi" 'dumb-jump-go-prompt
  "gx" 'dumb-jump-go-prefer-external
  "gz" 'dumb-jump-go-prefer-external-other-window
  "sb" 'gcl-baidu
  "sg" 'gcl-google
  "sh" 'gcl-github
  "sy" 'gcl-youtebe
  )
;; -END

;;----------------------------------------------------------------------------
;; `Semicolon Leader'
;;----------------------------------------------------------------------------
(semileader
  "1" 'avy-goto-word-0
  "2" 'avy-goto-word-1
  "3" 'avy-goto-line
  "`" 'avy-next)
;; -END

(provide 'init-general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-general.el ends here
