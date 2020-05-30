;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; `org'
;;----------------------------------------------------------------------------
(use-package org
  :demand
  :config
  ;; Supported languages in babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (emacs-lisp . t)
     (shell . t)))

  (setq org-catch-invisible-edits 'smart
        ;; Folders
        org-directory "~/projects"
        org-agenda-files (directory-files-recursively "~/projects" "\.org$")
        org-default-notes-file "~/projects/notes.org"
        ;; Startup
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-startup-indented t
        org-startup-truncated t
        org-hide-emphasis-markers t
        org-hide-block-startup t
        ;; Org sub/superscript behavior
        org-use-sub-superscripts '{}
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        ;; RET behavior
        org-M-RET-may-split-line nil
        org-return-follows-link nil
        org-image-actual-width 600
        ;; Generate UUIDs for links when storing link
        org-id-link-to-org-use-id t
        ;; Make todos dependent
        org-html-checkbox-type 'unicode
        org-enforce-todo-checkbox-dependencies nil
        org-enforce-todo-dependencies t
        ;; Log states
        org-log-into-drawer t
        ;; Recursive todo statistics
        org-hierarchical-todo-statistics nil
        ;; Todo keywords
        org-todo-keywords '((sequence "REFILE(r)" "SOMEDAY(s)" "TODO(t)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c@)"))
        ;; Colors for TODOS
        org-todo-keyword-faces
        '(("REFILE" . "red4")
          ("TODO" . "red")
          ("WAITING" . "orange")
          ("CANCELED" . (:strike-through t :foreground "DimGrey" :weight bold)))
        ;; Do not create a TOC
        org-export-with-toc nil
        ;; REFILE
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'full-file-path
        org-outline-path-complete-in-steps nil
        )

  ;; Capture templates
  (setq org-capture-templates
        '(("t"
           "todo"
           entry (file+headline org-default-notes-file "Tasks")
           "* REFILE %^{todo}\n %? \n %a")
          ("n"
           "note"
           entry (file+headline org-default-notes-file "Notes")
           "* %^{title}\n %?")
          ("C"
           "cheatsheet"
           entry (file+headline org-default-notes-file "Cheatsheets")
           "* %^{title}\n %?")))

  ;; Org windows
  (add-to-list 'display-buffer-alist '("*Org Agenda*"
                                       (display-buffer-reuse-window display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 80)
                                       (reusable-frames . nil)))

  ;; Project mode
  (add-to-list 'org-capture-templates
               '("p"
                 "project"
                 entry (file+headline "~/projects/projects.org" "projects")
                 "* [[%^{project}/notes.org][%^{project}]] %^g\n%^{TYPE}p %?\n"))

  ;; ;; Allow emphasis markup in the middle of words
  ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Strike-through for checked boxes
  (defface org-checkbox-done-text
    '((t (:foreground "DimGrey" :strike-through t)))
    "Face for text of checked boxes")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  ;; Menu keybindings
  :general
  (spcleader
    "o"  '(:ignore t :which-key "Org")
    "oO" 'projectorg/go-to-inbox
    "oo" 'projectorg/go-to-notes
    "oA" 'org-agenda
    "oa" 'counsel-projectile-org-agenda
    "oc" 'counsel-projectile-org-capture
    "ol" 'org-insert-link
    "oL" 'org-store-link
    "oi" 'org-redisplay-inline-images
    "os" 'org-sort
    "o*" 'org-toggle-heading
    "o/" 'org-sparse-tree
    "o|" 'org-table-create-or-convert-from-region
    "oe" 'org-table-edit-field
    "oE" 'org-export-dispatch
    "o}" 'org-table-toggle-coordinate-overlays
    "o{" 'org-table-toggle-formula-debugger
    "o?" 'org-table-field-info
    "o=" 'org-table-eval-formula
    "od" 'org-insert-drawer
    "ot" 'org-todo
    "op" 'org-set-property
    "oC" 'org-columns
    "of" 'org-footnote-action
    "ow" 'org-refile
    "oz" 'org-archive-subtree)

  ;; Normal keybindings
  (:keymaps  'org-mode-map
   :states   'normal
   "gt"      'org-mark-ring-goto
   "tt"      'org-todo
   "tc"      'org-toggle-checkbox
   "U"       'undo-tree-redo
   "<tab>"   'org-cycle
   "S-<tab>" 'org-global-cycle
   "M-s"     'org-metadown
   "M-d"     'org-metaup
   "M-r"     'org-metaright
   "M-t"     'org-metaleft
   "M-S"     'org-shiftmetadown
   "M-D"     'org-shiftmetaup
   "M-R"     'org-shiftmetaright
   "M-T"     'org-shiftmetaleft
   "C-s"     'org-shiftdown
   "C-d"     'org-shiftup
   "C-r"     'org-shiftright
   "C-t"     'org-shiftleft
   "©"       'org-babel-next-src-block
   "ſ"       'org-babel-previous-src-block)

  ;; Insert keybindings
  (:keymaps 'org-mode-map
   :states  '(normal insert)
   "Þ"      'outline-up-heading
   "Ð"      'outline-backward-same-level
   "ẞ"      'outline-forward-same-level
   "þ"      'outshine-kbd-M-<left>
   "®"      'outshine-kbd-M-<right>
   "ð"      'org-previous-visible-heading
   "ß"      'org-next-visible-heading)

  ;; Pretty checkboxes
  :hook (org-mode . (lambda ()
                      (push '("[ ]" . "☐") prettify-symbols-alist)
                      (push '("[X]" . "☑" ) prettify-symbols-alist)
                      (push '("[-]" . "☒" ) prettify-symbols-alist)
                      (setq-local prettify-symbols-unprettify-at-point 'right-edge)
                      (prettify-symbols-mode)))

  ;; Use ivy for org-link
  :hook (org-mode . (lambda () (setq-local completing-read-function 'ivy-completing-read)))

  :custom-face
  (highlight ((t (:background nil :foreground nil :box (:line-width 2 :color "dim gray") :DistantForeground nil))))
  (outline-1 ((t (:height 1.9 :background nil))))
  (outline-2 ((t (:height 1.6))))
  (outline-3 ((t (:height 1.4))))
  (outline-4 ((t (:height 1.2))))
  (outline-5 ((t (:height 1.1)))))
;; -END

;;----------------------------------------------------------------------------
;; `org-agenda'
;;----------------------------------------------------------------------------
(use-package org-agenda
  :ensure nil
  :config
  (setq
   ;; Hide blocked todos from agenda
   org-agenda-dim-blocked-tasks 'invisible
   ;; Agenda window
   org-agenda-window-setup 'current-window
   org-agenda-restore-windows-after-quit t
   ;; Agenda for 10 days from 1 day before
   org-agenda-span 10
   org-agenda-start-day "-1d"
   org-agenda-include-diary t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-tags-column -78
   org-agenda-start-with-follow-mode t
   org-agenda-todo-ignore-scheduled 'all
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-deadline-prewarning-if-scheduled 3
   ;; org-agenda-sorting-strategy
   ;; '((agenda deadline-up priority-down)
   ;;   (todo priority-down category-keep)
   ;;   (tags priority-down category-keep)
   ;;   (search category-keep))
   )

  ;; Agenda keybindings
  :general
  (:keymaps 'org-agenda-mode-map
   :states  '(normal)
   "q"      'org-agenda-quit
   "C-d"    'org-agenda-priority-up
   "C-s"    'org-agenda-priority-down
   "i"      'org-agenda-diary-entry
   "T"      'org-agenda-todo
   "ß"      'org-agenda-next-item
   "ð"      'org-agenda-previous-item
   "RET"    'org-agenda-goto
   "®"      'org-agenda-goto
   "_"      'org-agenda-cycle-show
   "C"      'org-agenda-columns))
;; -END


;;;;; Org notifications

(use-package org-wild-notifier
  :config
  (setq alert-default-style 'libnotify)
  ;; (org-wild-notifier-mode)
  )

;;;;; Org-bullets

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)

  :hook (org-mode . org-bullets-mode))

;;;;; Org fancy priorities

(use-package org-fancy-priorities
  :config
  ;; Set colors
  (setq org-priority-faces
        '((?A . "#ff5d38")
          (?B . "orange")
          (?C . "#98be65")))

  ;; Set chars
  (setq org-fancy-priorities-list
        '((?A . "⬆")
          (?B . "·")
          (?C . "⬇")))

  :hook (org-mode . org-fancy-priorities-mode))

;; TocOrgPac
(use-package toc-org
  :hook (org-mode . toc-org-mode))
;; -TocOrgPac

;; HTMLIZEPac
(use-package htmlize :defer t)
;; -HTMLIZEPac

;; OXGFMPac
(use-package ox-gfm :defer t)
;; -OXGFMPac

;; PlantUMLPac
(use-package plantuml-mode
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t))))
;; -PlantUMLPac

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
