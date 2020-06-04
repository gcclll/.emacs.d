;;; init-projectile.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `magit'
;;----------------------------------------------------------------------------
;; MagitPac
(use-package magit
  :if *git*
  :bind ("C-x g" . magit-status))
;; -MagitPac

;;----------------------------------------------------------------------------
;; `projectile'
;;----------------------------------------------------------------------------
(use-package projectile
  :diminish
  :bind
  ("C-c p" . projectile-command-map)
  ("C-z p" . projectile-add-known-project)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (when (and *sys/win32* *tr*)
    (setq projectile-indexing-method 'alien))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile
  :config
  (setq counsel-projectile-org-capture-templates
        '(("pt"
           "[${name}] Todo"
           entry (file+headline "${root}/notes.org" "Tasks")
           "* TODO %^{todo}\n %? \n %a")
          ("pn"
           "[${name}] Note"
           entry (file+headline "${root}/notes.org" "Notes")
           "* %^{title} %t\n %?")))
  :general
  (spcleader
    "p"     '(:ignore t :which-key "Projectile")
    "pf"    'counsel-projectile-find-file
    "pb"    'counsel-projectile-switch-to-buffer
    "pp"    'counsel-projectile-switch-project
    "pg"    'counsel-projectile-grep
    "p SPC" '(lambda () (interactive) (find-file "~/projects/projects.org"))))

;;----------------------------------------------------------------------------
;; `treemacs'
;;----------------------------------------------------------------------------
;; TreemacsPac
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
;; -TreemacsPac

;; TreeMagit
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
;; -TreeMagit

;; TreeProj
(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile))
;; -TreeProj

(provide 'init-projectile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
