;;; init-games.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `tetris'
;;----------------------------------------------------------------------------
(use-package tetris
  :ensure nil
  :commands (tetris)
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-n" . tetris-rotate-down)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-SPC" . tetris-move-bottom))
  :config
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it)))
;; -END

;;----------------------------------------------------------------------------
;; `speed-type'
;;----------------------------------------------------------------------------
(use-package speed-type
  :commands (speed-type-text))
;; -END

;;----------------------------------------------------------------------------
;; `2048-game'
;;----------------------------------------------------------------------------
(use-package 2048-game
  :commands (2048-game))
;; -END

(provide 'init-games)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-games.el ends here
