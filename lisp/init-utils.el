;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

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

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
              ([remap kill-buffer] . (lambda (&rest _)
                                       (interactive)
                                       (user-error "Scrach buffer cannot be killed")))
              ([remap revert-buffer] . persistent-scratch-restore)
              ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu)
              :map rg-mode-map
              ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))

;; Dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c D" . osx-dictionary-search-pointer))))

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun my-youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (if emacs/>=26p
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point)))
  :config
  (with-eval-after-load 'hydra
    (defhydra youdao-dictionary-hydra (:color blue)
      ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
      ("y" youdao-dictionary-play-voice-at-point "play voice at point")
      ("q" quit-window "quit")
      ("C-g" nil nil)
      ("h" nil nil)
      ("?" nil nil))))

;; A Simple and cool pomodoro timer
(use-package pomidor
  :bind ("s-<f12>" . pomidor)
  :init
  (setq alert-default-style 'mode-line)

  (with-eval-after-load 'all-the-icons
    (setq alert-severity-faces
          '((urgent   . all-the-icons-red)
            (high     . all-the-icons-orange)
            (moderate . all-the-icons-yellow)
            (normal   . all-the-icons-green)
            (low      . all-the-icons-blue)
            (trivial  . all-the-icons-purple))
          alert-severity-colors
          `((urgent   . ,(face-foreground 'all-the-icons-red))
            (high     . ,(face-foreground 'all-the-icons-orange))
            (moderate . ,(face-foreground 'all-the-icons-yellow))
            (normal   . ,(face-foreground 'all-the-icons-green))
            (low      . ,(face-foreground 'all-the-icons-blue))
            (trivial  . ,(face-foreground 'all-the-icons-purple)))))

  (when sys/macp
    (setq pomidor-play-sound-file
          (lambda (file)
            (when (executable-find "afplay")
              (start-process "pomidor-play-sound" nil "afplay" file))))))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . (lambda ()
                                      "Enter edit mode and delete other windows."
                                      (and (fboundp 'olivetti-mode)
                                           (olivetti-mode 1))
                                      (delete-other-windows))))
  :init (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))))

;; Music player
(use-package bongo
  :bind ("C-<f9>" . bongo)
  :config
  (with-eval-after-load 'dired
    (with-no-warnings
      (defun bongo-add-dired-files ()
        "Add marked files to the Bongo library."
        (interactive)
        (bongo-buffer)
        (let (file (files nil))
          (dired-map-over-marks
           (setq file (dired-get-filename)
                 files (append files (list file)))
           nil t)
          (with-bongo-library-buffer
           (mapc 'bongo-insert-file files)))
        (bongo-switch-buffers))
      (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

;; IRC
(use-package erc
  :ensure nil
  :defines erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; A stackoverflow and its sisters' sites reader
(when emacs/>=26p
  (use-package howdoyou
    :bind (:map howdoyou-mode-map
					 ("q" . kill-buffer-and-window))
    :hook (howdoyou-mode . read-only-mode)))

;;----------------------------------------------------------------------------
;; `crux'
;;----------------------------------------------------------------------------
(use-package crux
  ;; :bind
  ;; (("C-a" . crux-move-beginning-of-line)
  ;; ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))
;; -END

;;----------------------------------------------------------------------------
;; `zoom'
;;----------------------------------------------------------------------------
(defun size-callback ()
  "Zoom."
  (cond ((> (frame-pixel-width) 1280) '(0.75 . 0.75))
        (t                            '(0.5 . 0.5))))
(defun my/fix-imenu-size ()
  "Fix imenu size."
  (with-selected-window (get-buffer-window "*lsp-ui-imenu*") ;; Ilist
    (setq window-size-fixed t)
    (window-resize (selected-window) (- 30 (window-total-width)) t t)))

(use-package zoom
  :config
  (zoom-mode t)
  (setq zoom-size 'size-callback)
  (setq zoom-ignored-major-modes '(dired-mode markdown-mode lsp-ui-imenu-mode imenu-mode))
  (add-hook 'imenu-list-update-hook 'my/fix-imenu-size))
;; -END

;;----------------------------------------------------------------------------
;; `smartparens'
;;----------------------------------------------------------------------------
(use-package smartparens
  :ensure t
	:init
	(bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  (bind-key "C-S-s" #'sp-splice-sexp)
  (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))
  :config
	(smartparens-global-mode t)
	(sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)
;; -END



;;----------------------------------------------------------------------------
;; `misc'
;;----------------------------------------------------------------------------
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)
(use-package memory-usage)
(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  (use-package tldr))
(use-package emojify
  :hook (after-init . global-emojify-mode))
(use-package ranger)
(use-package restart-emacs)
(use-package move-text)
(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode t))
(use-package helm-dash)
;; -END


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
