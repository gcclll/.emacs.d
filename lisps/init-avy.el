;;; init-avy.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-global))

;; AvyPac
(use-package avy
  :defer t
  :bind
  (("C-z c" . avy-goto-char-timer)
   ("C-z l" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))));
;; -AvyPac

(use-package ace-window
  :ensure t
  :defer t
  :config
  (set-face-attribute
    'aw-leading-char-face nil
    :foreground "deep sky blue"
    :weight 'bold
    :height 3.0)
  (set-face-attribute
    'aw-mode-line-face nil
    :inherit 'mode-line-buffer-id
    :foreground "lawn green"))

(provide 'init-avy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-avy.el ends here