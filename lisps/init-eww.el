;;; init-eww.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-defs))

;; EWWPac
(use-package eww
  :ensure nil
  :commands (eww)
  :hook (eww-mode . (lambda ()
                      "Rename EWW's buffer so sites open in new page."
                      (rename-buffer "eww" t)))
  :config
  ;; I am using EAF-Browser instead of EWW
  (unless *eaf-env*
    (setq browse-url-browser-function 'eww-browse-url))) ; Hit & to browse url with system browser
;; -EWWPac

(provide 'init-eww)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eww.el ends here