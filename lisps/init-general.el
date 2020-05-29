;;; init-general.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-defs)
  (require 'init-global))

(use-package general
 :commands general-override-states
 :init
 (setq general-override-states '(insert
                                 emacs
                                 hybrid
                                 normal
                                 visual
                                 motion
                                 operator
                                 replacea))
 :config
 (general-evil-setup t))

;;----------------------------------------------------------------------------
;; `SPC Leader'
;;----------------------------------------------------------------------------
 (general-create-definer my-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

  (my-space-leader-def
    "SPC" 'counsel-M-x
    ;; <b>
    "bb" 'counsel-switch-buffer
    "bd" 'kill-this-buffer
    "bi" 'ibuffer
    "bf" 'gcl/open-in-finder
    "bK" 'crux-kill-other-buffers
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "br" 'counsel-buffer-or-recentf
    "bs" 'save-buffer
    "bS" 'save-all-buffers
    ;; <d>
    ;; "dd" 'dash-at-piont
    ;; <f>
    "ff" 'counsel-find-file
    "fp" 'ffip
    "fR" 'gcl/rename-this-file-and-buffer
    ;; <g>
    "gg" 'magit-status
    ;; <o>
    "om" 'lsp-ui-imenu
    ;; <q>
    "qr" 'restart-emacs
    ;; <s>
    "ss" 'swiper
    "sS" 'swiper-all
    "sr" 'counsel-rg
    "sg" 'counsel-git-grep
    "sp" 'rg-project
    "sq" 'query-replace
    "sQ" 'query-replace-regexp
    ;; <w>
    "wv" 'split-window-horizontally
    "w-" 'split-window-vertically
    "wl" 'evil-window-right
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
;; `JavaScript Leader'
;;----------------------------------------------------------------------------
(general-create-definer my-javascript-leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal motion insert emacs)
  :keymaps 'js2-mode-map)
;; -END

;;----------------------------------------------------------------------------
;; `Comma Leader'
;;----------------------------------------------------------------------------
(general-create-definer my-comma-leader-def
  :prefix ","
  :states '(normal visual))

(my-comma-leader-def
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
(general-create-definer my-semicolon-leader-def
  :prefix ";"
  :states '(normal visual))
;; -END

(provide 'init-general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-general.el ends here
