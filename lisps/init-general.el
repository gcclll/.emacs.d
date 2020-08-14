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
  "fe" 'crux-find-user-init-file
  "fp" 'ffip
  "fr" 'crux-recentf-find-file
  "fR" 'crux-rename-file-and-buffer
  "fd" 'crux-delete-file-and-buffer
  ;; <g>
  "gg" 'magit-status
  "gb" 'gcl/bakup
  "gp" 'gcl/git-push
  ;; <h>
  "hr" 'hydra-rectangle/body
  "hm" 'hydra-multiple-cursors/body
  "ho" 'hydra-org-agenda/body
  ;; <i>
  "ie" 'emojify-insert-emoji
  "im" 'imenu
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
  "gx" 'dumb-jump-go-prefer-external
  "gz" 'dumb-jump-go-prefer-external-other-window
  "sb" 'engine/search-baidu
  "sd" 'engine/search-duckduckgo
  "sg" 'engine/search-github
  "si" 'engine/search-google-images
  "sm" 'engine/search-google-maps
  "sr" 'engine/search-rfcs
  "ss" 'engine/search-amazon
  "st" 'engine/search-twitter
  "sy" 'engine/search-youtube
  "sw" 'engine/search-wikipedia
  "sB" 'engine/search-books
  "sG" 'engine/search-google
  "sM" 'engine/search-melpa
  "sS" 'engine/search-stack-overflow
  ;; restclient
  "rc" 'restclient-http-send-current
  "rr" 'restclient-http-send-current-raw
  "rv" 'restclient-http-send-current-stay-in-window
  "rn" 'restclient-jump-next
  "rp" 'restclient-jump-previous
  "r." 'restclient-mark-current
  "rC" 'restclient-copy-curl-command
  "rN" 'restclient-narrow-to-current
  "ra" 'restclient-toggle-body-visibility
  "ri" 'restclient-show-info
  )
;; -END

;;----------------------------------------------------------------------------
;; `Semicolon Leader'
;;----------------------------------------------------------------------------
(semileader
  "1" 'avy-goto-word-0
  "2" 'avy-goto-word-1
  "3" 'avy-goto-line
  "`" 'avy-next
  )

;; -END

;;----------------------------------------------------------------------------
;; `backquote Leader'
;;----------------------------------------------------------------------------
(bqleader
  "`" 'evil-goto-mark
  "b" 'dumb-jump-back
  "g" 'dumb-jump-go
  "j" 'dumb-jump-go-prompt
  "TAB" 'indent-rigidly
  )

(bqleader
  :keymaps 'dashboard-mode-map
  "n" 'dashboard-next-section
  "p" 'dashboard-previous-section)
;; -END

(provide 'init-general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-general.el ends here
