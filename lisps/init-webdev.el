;;; init-webdev.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WebModePac
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))
;; -WebModePac

;;----------------------------------------------------------------------------
;; `js2-mode'
;;----------------------------------------------------------------------------
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-strict-missing-semi-warning nil
                js-switch-indent-offset 2
                js-indent-level 2
                js2-basic-offset 2)
  (use-package add-node-modules-path
    :after typescript-mode js2-mode
    :config
    (add-hook 'typescript-mode-hook 'add-node-modules-path)
    (add-hook 'js2-mode-hook 'add-node-modules-path)))
;; -END

;;----------------------------------------------------------------------------
;; `typescript-mode'
;;----------------------------------------------------------------------------
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode)
  :config
  (setq-default typescript-indent-level 2))
;; -END

;; EmmetPac
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
;; -EmmetPac

;; InstantRenameTagPac
(use-package instant-rename-tag
  :load-path (lambda () (expand-file-name "site-elisp/instant-rename-tag" user-emacs-directory)))
;; -InstantRenameTagPac

;; JsonPac
(use-package json-mode
  :mode "\\.json\\'")
;; -JsonPac

;;----------------------------------------------------------------------------
;; `prettier-js'
;;----------------------------------------------------------------------------
(use-package prettier-js
  :diminish prettier-js-mode
  :commands (prettier-js-mode prettier)
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  ;; (add-hook 'vue-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-target-mode "js2-mode")
  (setq prettier-js-args '(
                           "--trailing-comma" "none"
                           "--print-width" "80"
                           "--tab-width" "2"
                           "--single-quote" "true"
                           "--no-semi"
                           ))
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode
                                '("\\.jsx?\\'" . prettier-js-mode)))))

(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
