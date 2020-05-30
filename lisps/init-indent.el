;;; init-indent.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `highlight-indent-guides'
;;----------------------------------------------------------------------------
(use-package highlight-indent-guides
  :if *sys/gui*
  :diminish
  ;; Enable manually if needed, it a severe bug which potentially core-dumps Emacs
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/76
  :commands (highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))
;; -END


;;----------------------------------------------------------------------------
;; `smart-electric-indent-mode'
;;----------------------------------------------------------------------------
(defun smart-electric-indent-mode ()
  "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode)))
         (electric-indent-mode 0))
        ((eq electric-indent-mode nil) (electric-indent-mode 1))))
;; -END


;; -IndentConfig

(provide 'init-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-indent.el ends here
