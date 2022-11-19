
(defun reload-init-file ()
    "Reload init file with <f5>."
    (interactive)
    (load-file (expand-file-name "init.el" gcl-emacs-config-dir)))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (and (file-readable-p custom-file) (load custom-file))

  (defun gcl/open-init-file()
    (interactive)
    (find-file (expand-file-name "init.el" gcl-emacs-config-dir)))

  (global-set-key (kbd "<f5>") 'reload-init-file)
  (global-set-key (kbd "<f1>") 'gcl/open-init-file)
  (global-set-key (kbd "<f2>") 'restart-emacs)

  ;; 保存文件时自动生成配置到 init.el
  (use-package org-auto-tangle
    :hook (org-mode . org-auto-tangle-mode))

  (use-package auto-save
    :straight (:host github :repo "manateelazycat/auto-save")
    :config
    (auto-save-enable)
    (setq auto-save-silent t)
    (setq auto-save-delete-trailing-whitespace t)
    ;; 不想自动保存的文件后缀
    (setq auto-save-disable-predicates
	'((lambda ()
	(string-suffix-p
	"gpg"
	(file-name-extension (buffer-name)) t))))
    )

(provide 'init-hacking)