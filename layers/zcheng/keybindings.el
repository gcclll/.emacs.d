;;; keybindings.el --- summary -*- lexical-binding: t -*-

;; Author: Zhicheng Lee
;; Maintainer: Zhicheng Lee
;; Email: gccll.love@gmail.com
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://www.zcheng.top
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

;;----------------------------------------------------------------------------
;; `func'
;;----------------------------------------------------------------------------
;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))
;; -END



;;----------------------------------------------------------------------------
;; `global-key'
;;----------------------------------------------------------------------------

(define-key evil-normal-state-map (kbd "z(") 'sp-wrap-round)
(define-key evil-normal-state-map (kbd "z[") 'sp-wrap-square)
(define-key evil-normal-state-map (kbd "z{") 'sp-wrap-curly)
(define-key evil-normal-state-map (kbd "z-") 'sp-splice-sexp)
(define-key evil-normal-state-map (kbd "z.") 'emmet-wrap-with-markup)
(define-key evil-visual-state-map (kbd "z.") 'emmet-wrap-with-markup)

;; -END

;;----------------------------------------------------------------------------
;; `define-key'
;;----------------------------------------------------------------------------
(define-key global-map (kbd "C-c t") 'org-capture)
;; -END

;;----------------------------------------------------------------------------
;; `bind-key'
;;----------------------------------------------------------------------------
(bind-key* "C-=" 'er/expand-region)
(bind-key* "C-c l" 'zilongshanren/insert-chrome-current-tab-url)
(bind-key* "M--" 'zilongshanren/goto-match-paren)
(bind-key* "M-i" 'string-inflection-java-style-cycle)
(bind-key* "s-p" 'find-file-in-project)

;; -END



;;----------------------------------------------------------------------------
;; `evil-key'
;;----------------------------------------------------------------------------
(bb/define-key evil-normal-state-map
               "+" 'evil-numbers/inc-at-pt
               "-" 'evil-numbers/dec-at-pt
               "\\" 'evil-repeat-find-char-reverse)
;; -END

;;----------------------------------------------------------------------------
;; `spacemacs'
;;----------------------------------------------------------------------------
(spacemacs/set-leader-keys "aS" 'prodigy)

(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/set-leader-keys "bM" 'spacemacs/switch-to-messages-buffer)
(spacemacs/set-leader-keys "bm" nil)
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)
(spacemacs/set-leader-keys "bmj" 'counsel-bookmark)

(spacemacs/set-leader-keys "dd" 'dash-at-point)

(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "fd" 'projectile-find-file-dwim-other-window)

;; highlight
(spacemacs/set-leader-keys "hh" 'zilongshanren/highlight-dwim)
;; (spacemacs/set-leader-keys "hc" 'zilongshanren/clearn-highlight)
(spacemacs/set-leader-keys "hc" 'symbol-overlay-remove-all)

(spacemacs/set-leader-keys "gg" 'magit)
(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)
(spacemacs/set-leader-keys "gn" 'smerge-next)
(spacemacs/set-leader-keys "gp" 'smerge-prev)
(spacemacs/set-leader-keys "gP" 'gcl/git-push)



(global-set-key (kbd "<f1>") 'zilongshanren/helm-hotspots)
(spacemacs/set-leader-keys "og" 'my-git-timemachine)
(spacemacs/set-leader-keys "oll" 'zilongshanren/load-my-layout)
(spacemacs/set-leader-keys "ols" 'zilongshanren/save-my-layout)
(spacemacs/set-leader-keys "oo" 'zilongshanren/helm-hotspots)
(spacemacs/set-leader-keys "or" 'zilongshanren/browser-refresh--chrome-applescript)
(spacemacs/set-leader-keys "os" 'spacemacs/search-engine-select)
;; (spacemacs/set-leader-keys "oS" 'sunshine-forecast)

(spacemacs/set-leader-keys "ox" 'org-open-at-point-global)
(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "o!" 'zilongshanren/iterm-shell-command))

(spacemacs/set-leader-keys "xe" 'set-buffer-file-coding-system)

(spacemacs/set-leader-keys "yi" 'yas/insert-snippet)
(spacemacs/set-leader-keys "yd" 'youdao-dictionary-search-at-point+)

;; -END

;;----------------------------------------------------------------------------
;; `other'
;;----------------------------------------------------------------------------
(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")
;; -END

(provide 'keybindings)

;;; keybindings.el ends here
