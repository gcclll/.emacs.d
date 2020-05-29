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

(provide 'init-avy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-avy.el ends here