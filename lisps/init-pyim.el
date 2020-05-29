;;; init-pyim.el --- -*- lexical-binding: t -*-
;;; Code:

;; PyimPac
(use-package pyim
  :init
  (use-package posframe :defer t)
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 9)
  :config
  (pyim-isearch-mode 1)
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  ("M-j" . pyim-convert-string-at-point)) ; M-j 强制将光标前的拼音字符串转换为中文。
;; -PyimPac

;; PyimBaseDictPac
(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))
;; -PyimBaseDictPac

(provide 'init-pyim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pyim.el ends here