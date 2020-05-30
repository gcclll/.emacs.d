;;; init-epaint.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `epaint'
;;----------------------------------------------------------------------------
(use-package epaint
  :if *sys/gui*
  :load-path (lambda () (expand-file-name "site-elisp/epaint" user-emacs-directory))
  :commands (epaint)
  :init
  (with-eval-after-load (quote epaint-context)
    (unless (boundp (quote cl-struct-epaint-drawable))
      (defvar cl-struct-epaint-drawable (quote epaint-drawable)))
    (unless (boundp (quote cl-struct-epaint-gc))
      (defvar cl-struct-epaint-gc (quote epaint-gc)))))
;; -END

(provide 'init-epaint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-epaint.el ends here
