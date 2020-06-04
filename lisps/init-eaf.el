;;; init-eaf.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `eaf'
;;----------------------------------------------------------------------------

;;; cannot support MacOS:
;;; https://github.com/manateelazycat/emacs-application-framework/issues/10#issuecomment-401603088
(use-package eaf
  :load-path (lambda () (expand-file-name "site-elisp/emacs-application-framework" user-emacs-directory))
  ;; :if *eaf-env*
  :custom
  (eaf-find-alternate-file-in-dired t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (setq eaf-proxy-type "socks5")
  (setq eaf-grip-token "43697da598407cbfec5eaa32bf4b2a4790652df6")
  (eaf-setq eaf-browser-default-zoom "1.25")
  (eaf-setq eaf-browser-dark-mode "false")
  (eaf-bind-key scroll_up "RET" eaf-pdfviewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdfviewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdfviewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdfviewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  )

;; -END

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
