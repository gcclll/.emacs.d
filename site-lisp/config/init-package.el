(setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
			             ("melpa" . "http://1.15.88.122/melpa/")))

 (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Set up use-package for tidier package configuration/installation
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (setq straight-vc-git-default-clone-depth 1)

  ;; Add diminish, which makes it easier to customize lighters (minor mode display)
  (use-package diminish)

(provide 'init-package)