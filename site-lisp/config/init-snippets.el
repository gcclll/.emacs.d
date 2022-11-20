(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(define-abbrev-table
  'global-abbrev-table '(
			 ;; signature
			 ("8zc" "lizhicheng")
			 ("8zj" "李志诚")
			 ("8lp" "范婷婷")
			 ;; Microsoft
			 ("8ms" "Microsoft")
			 ("8em" "gccll.love@gmail.com")
			 ("8bl" "https://blog.cheng92.com")
			 ))

;; --- abbrev
(use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
	   ("C-M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))

;; --- fancy
(use-package fancy-dabbrev
  :ensure t
  :config
  (global-fancy-dabbrev-mode)
  ;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
  ;; choice, here "TAB" and "Shift+TAB":
    (global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
    (global-set-key (kbd "M-?") 'fancy-dabbrev-backward)

  ;; If you want TAB to indent the line like it usually does when the cursor
  ;; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
  ;; instead of `fancy-dabbrev-expand`:
  ;; (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
  ;; (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
  ;; Let dabbrev searches ignore case and expansions preserve case:
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  )

;; --- bindings
(general-define-key
 "M-/" 'fancy-dabbrev-expand
 "M-?" 'fancy-dabbrev-backward
 "C-M-/" 'dabbrev-expand
 )

;; --- snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/site-lisp/snippets"))
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  )

(provide 'init-snippets)
