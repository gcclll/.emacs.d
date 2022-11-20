(require 'perspective)

(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))

(setq persp-state-default-file (expand-file-name ".cache/gcl" user-emacs-directory))
(setq persp-show-modestring 'header)
(add-hook 'kill-emacs-hook #'persp-state-save)
(persp-mode)

(require 'persp-projectile)

(with-eval-after-load 'general
  (general-define-key
   "s-1" '(lambda () (interactive) (persp-switch-by-number 1))
   "s-2" '(lambda () (interactive) (persp-switch-by-number 2))
   "s-3" '(lambda () (interactive) (persp-switch-by-number 3))
   "s-4" '(lambda () (interactive) (persp-switch-by-number 4))
   "s-5" '(lambda () (interactive) (persp-switch-by-number 5))
   "s-)" 'persp-next
   "s-(" 'persp-prev
   ))

(provide 'init-persp)
