;;; init-misc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `recentf'
;;----------------------------------------------------------------------------
(use-package recentf
  :diminish
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))
;; -END

;;----------------------------------------------------------------------------
;; `winner'
;;----------------------------------------------------------------------------
(use-package winner
  :diminish
  :ensure nil
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))
;; -END

;;----------------------------------------------------------------------------
;; `which-key'
;;----------------------------------------------------------------------------
(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))
;; -END

;;----------------------------------------------------------------------------
;; `undo-tree'
;;----------------------------------------------------------------------------
(use-package undo-tree
  :diminish
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
;; -END

;;----------------------------------------------------------------------------
;; `two'
;;----------------------------------------------------------------------------
(use-package sudo-edit
  :diminish
  :commands (sudo-edit))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

(use-package rainbow-mode
  :diminish
  :demand
  :config
  (rainbow-mode))
;; -END

;;----------------------------------------------------------------------------
;; `zone'
;;----------------------------------------------------------------------------
(use-package zone
  :diminish
  :ensure nil
  :defer 5
  :config
  ;; (zone-when-idle 600) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))
;; -END


;;----------------------------------------------------------------------------
;; `smart-tab'
;;----------------------------------------------------------------------------
(use-package smart-tab
  :diminish
  :config
  (progn
    (defun @-enable-smart-tab ()
      (smart-tab-mode))
    (add-hook 'prog-mode-hook '@-enable-smart-tab)
    ))
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
;; `editor'
;;----------------------------------------------------------------------------
(use-package editorconfig
  :diminish
  :defer 0.3
  :config (editorconfig-mode 1))
;; -END

;;----------------------------------------------------------------------------
;; `electric-operator'
;;----------------------------------------------------------------------------
(use-package electric-operator
  :diminish
  :delight
  :hook (python-mode . electric-operator-mode))
;; -END

;;----------------------------------------------------------------------------
;; `simple'
;;----------------------------------------------------------------------------
(use-package simple
  :diminish
  :ensure nil
  :delight (auto-fill-function)
  :bind ("C-x p" . pop-to-mark-command)
  :hook ((before-save . delete-trailing-whitespace)
         (prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :custom (set-mark-command-repeat-pop t))
;; -END

;;----------------------------------------------------------------------------
;; `none'
;;----------------------------------------------------------------------------
(use-package ace-jump-mode)
(use-package avy)
(use-package ranger)
(use-package windmove)
(use-package buffer-move)
(use-package restart-emacs)
(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode t))
(use-package move-text)

;; -END



(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
