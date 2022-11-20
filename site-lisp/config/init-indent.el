(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defun adjust-languages-indent (n)
  (setq-local c-basic-offset n)
  ;; sh
  (setq-local sh-basic-offset n)
  ;; (setq-local sh-indentation n)
  (setq-local smie-indent-basic n)

  (setq-local coffee-tab-width n)
  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)
  (setq-local typescript-indent-offset n)
  (setq-local typescript-indent-level n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'sh-mode-hook
               'haskell-cabal-mode-hook
               'ruby-mode-hook
               'qml-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
	             'sh-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 4)
                     )))

(dolist (hook (list
               'web-mode-hook
               'js-mode-hook
	             'scss-mode-hook
               'typescript-mode-hook
               'css-mode-hook
               'js2-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 2)
                     )))

(provide 'init-indent)

;;; init-indent.el ends here
