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

(defun reload-init-file ()
  "Reload init file with <f5>."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(and (file-readable-p custom-file) (load custom-file))

(defun gcl/open-init-file()
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(global-set-key (kbd "<f5>") 'reload-init-file)
(global-set-key (kbd "<f1>") 'gcl/open-init-file)
(global-set-key (kbd "<f2>") 'restart-emacs)
