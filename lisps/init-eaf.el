;;; init-eaf.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile
  (require 'init-defs))

;; EAFPac
(use-package eaf
  :load-path (lambda () (expand-file-name "site-elisp/emacs-application-framework" user-emacs-directory))
  :if *eaf-env*
  :custom
  (eaf-find-alternate-file-in-dired t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-setq eaf-browser-default-zoom "1.25")
  (eaf-setq eaf-browser-dark-mode "false")
  ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
  ;; Comment this line out of you don't want to use EAF to open available files in dired.
  ;; (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_home "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key eaf-send-key-sequence "M-]" eaf-terminal-keybinding))
;; -EAFPac


(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here