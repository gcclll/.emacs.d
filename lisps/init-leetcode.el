;;; init-leetcode.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-defs))

;; LeetCodePac
(use-package leetcode
  :load-path (lambda () (expand-file-name "site-elisp/leetcode.el" user-emacs-directory))
  :commands (leetcode)
  :init
  (use-package graphql :defer t)
  (use-package aio :defer t)
  :custom
  (url-debug t)
  (leetcode-prefer-language "python3"))
;; -LeetCodePac

(provide 'init-leetcode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-leetcode.el ends here