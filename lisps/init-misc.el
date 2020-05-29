;;; init-misc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WinnerPac
(use-package winner
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
;; -WinnerPac

;; WhichKeyPac
(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))
;; -WhichKeyPac

;; PopKillRing
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))
;; -PopKillRing

;; UndoTreePac
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
;; -UndoTreePac

;; DiscMyMajor
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))
;; -DiscMyMajor

;; AceWindowPac
(use-package ace-window
  :bind ("C-x C-o" . ace-window))
;; -AceWindowPac

;; ZonePac
(use-package zone
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
;; -ZonePac

(use-package rainbow-mode
  :demand
  :config
  (rainbow-mode))

(use-package smart-tab
  :config
  (progn
    (defun @-enable-smart-tab ()
      (smart-tab-mode))
    (add-hook 'prog-mode-hook '@-enable-smart-tab)
    ))

(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
