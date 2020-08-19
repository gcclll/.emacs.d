;;; init-keybinds.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `SPC Leader'
;;----------------------------------------------------------------------------
(spcleader
  "SPC" 'counsel-M-x
  ;;   ;; <a>
  ;;   "a~" 'shell-here
  ;;   "a3" 'aweshell-dedicated-open
  ;;   "a#" 'aweshell-dedicated-close
  ;;   "a$" 'multi-term
  "ar" 'ranger
  ;;   ;; <c>
  ;;   ;; <b>
  "bb" 'counsel-switch-buffer
  "bd" 'kill-this-buffer
  "bi" 'ibuffer
  "bf" 'gcl/open-in-finder
  "bK" 'crux-kill-other-buffers
  "bo" 'crux-open-with
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "br" 'counsel-buffer-or-recentf
  "bm" 'buffer-menu
  ;; <c>
  "cw" 'counsel-colors-web
  ;;   "cp" 'crux-duplicate-current-line-or-region
  ;;   ;; <d>
  ;;   "do" 'open-dashboard
  "dd" 'dash-at-point
  ;;   ;; <f>
  "fb" 'gcl/open-in-finder
  "ff" 'counsel-find-file
  "fD" 'gcl/delete-this-file
  "fe" 'crux-find-user-init-file
  "fo" 'gcl/open-in-external-app
  "fp" 'ffip
  "fr" 'crux-recentf-find-file
  "fR" 'crux-rename-file-and-buffer
  "fd" 'crux-delete-file-and-buffer
  ;; <g>
  "gg" 'magit-status
  "gb" 'gcl/bakup
  "gp" 'gcl/git-push
  ;;   ;; <i>
  "ie" 'emojify-insert-emoji
  "im" 'imenu
  ;; <l>, load
  "la" 'counsel-osx-app
  "lc" 'counsel-world-clock
  "ls" 'counsel-tramp ; ssh list
  "lC" 'counsel-colors-web
  ;; <m>, mode
  "mi" 'iedit-mode
  "mI" 'iedit-rectangle-mode
  "ms" 'bookmark-set
  ;;   ;; <o>
  ;;   "om" 'lsp-ui-imenu
  ;; <p>
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  ;;   ;; <q>
  "qr" 'restart-emacs
  ;; <r>
  "rr" 'quickrun
  ;;   "rt" 'instant-rename-tag
  ;; <s>
  "sa" 'swiper-all
  "sb" 'swiper-isearch-backward
  "si" 'swiper-isearch
  "sg" 'counsel-git-grep
  "sp" 'rg-project
  "sq" 'query-replace
  "sQ" 'query-replace-regexp
  "sr" 'ivy-resume
  "sR" 'counsel-rg
  "ss" 'swiper
  "sz" 'counsel-fzf
  ;; <t>
  "tb" 'treemacs-bookmark
  "tf" 'treemacs-find-file
  "tt" 'treemacs
  "tT" 'treemacs-find-tag
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
  ;; "yd" 'my-you
  "yd" 'youdao-dictionary-search-at-point+
  "yD" 'my-youdao-dictionary-search-at-point
  "yi" 'yas-insert-snippet
  "yn" 'yas-new-snippet
  ;;   <u>
  "u." 'browse-url-at-point
  "ub" 'browse-url-of-buffer
  "uc" 'counsel-unicode-char
  "ur" 'browse-url-of-region
  "uu" 'browse-url
  "ue" 'browse-url-emacs
  "uf" 'browse-url-of-file
  ;; <v>
  "vp" 'ivy-push-view
  "vo" 'ivy-pop-view
  "v." 'ivy-switch-view
  )
;; -END


;;----------------------------------------------------------------------------
;; `Comma Leader'
;;----------------------------------------------------------------------------
(comaleader
  ;; <g>, jump
  "gb" 'xref-pop-marker-stack
  "gi" 'dumb-jump-go-prompt
  "gj" 'xref-find-definitions
  ;; "go" 'dumb-jump-go-other-window
  ;; "gx" 'dumb-jump-go-prefer-external
  ;; "gz" 'dumb-jump-go-prefer-external-other-window
  ;; <l>, lsp
  "li" 'lsp-ui-imenu
  ;;   "sb" 'engine/search-baidu
  ;;   "sd" 'engine/search-duckduckgo
  ;;   "sg" 'engine/search-github
  ;;   "si" 'engine/search-google-images
  ;;   "sm" 'engine/search-google-maps
  ;;   "sr" 'engine/search-rfcs
  ;;   "ss" 'engine/search-amazon
  ;;   "st" 'engine/search-twitter
  ;;   "sy" 'engine/search-youtube
  ;;   "sw" 'engine/search-wikipedia
  ;;   "sB" 'engine/search-books
  ;;   "sG" 'engine/search-google
  ;;   "sM" 'engine/search-melpa
  ;;   "sS" 'engine/search-stack-overflow
  ;;   ;; restclient
  ;;   "rc" 'restclient-http-send-current
  ;;   "rr" 'restclient-http-send-current-raw
  ;;   "rv" 'restclient-http-send-current-stay-in-window
  ;;   "rn" 'restclient-jump-next
  ;;   "rp" 'restclient-jump-previous
  ;;   "r." 'restclient-mark-current
  ;;   "rC" 'restclient-copy-curl-command
  ;;   "rN" 'restclient-narrow-to-current
  ;;   "ra" 'restclient-toggle-body-visibility
  ;;   "ri" 'restclient-show-info
  )
;; -END

;;----------------------------------------------------------------------------
;; `Semicolon Leader'
;;----------------------------------------------------------------------------
(semileader
	;; nums
  "0" 'avy-goto-word-0
  "1" 'avy-goto-word-1
  "2" 'avy-goto-line
	;; sign
  ";" 'avy-goto-char
  "'" 'avy-goto-char-2
  "," 'avy-next
	;; a-z, A-Z
	"b" 'xref-pop-marker-stack
  "d" 'avy-zap-to-char-dwim
  "D" 'avy-zap-up-to-char-dwim
	"i" 'dumb-jump-go-prompt
	"j" 'xref-find-definitions
  "o" 'ace-link-addr
  "z" 'zap-to-char
  "Z" 'zap-up-to-char
  "w" 'mark-word
  "W" 'mark-sexp
  )

;; -END

;;----------------------------------------------------------------------------
;; `backquote Leader'
;;----------------------------------------------------------------------------
(bqleader
  ;;   "TAB" 'indent-rigidly
  )

;; -END

;;----------------------------------------------------------------------------
;; `backquote Leader'
;;----------------------------------------------------------------------------
(zleader
 "(" 'sp-wrap-round ; wrap with ()
 "[" 'sp-wrap-square ; wrap with []
 "{" 'sp-wrap-curly ; wrap with {}
 "-" 'sp-splice-sexp ; unwrap
 )

;; -END



(provide 'init-keybinds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybinds.el ends here
