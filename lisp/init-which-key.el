;; init-which-key.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Some useful Utilities.
;;

;;; Code:

(require 'init-const)

;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t)
  :config
  (which-key-add-key-based-replacements "SPC b" "buffer")
  (which-key-add-key-based-replacements "SPC f" "file&project")
  (which-key-add-key-based-replacements "SPC l" "launch-app")
  (which-key-add-key-based-replacements "SPC s" "search-engine")
  (which-key-add-key-based-replacements "SPC t" "treemacs-...")
  (which-key-add-key-based-replacements "SPC w" "window")
  (which-key-add-key-based-replacements "SPC u" "url")
  (which-key-add-key-based-replacements "SPC v" "view")
  (which-key-add-key-based-replacements ";" "avy-zap-mark")

  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c c" "counsel")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c v" "ivy-view")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")


  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c ," "overseer")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-x" "markdown-toggle"))

(provide 'init-which-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-which-key.el ends here
