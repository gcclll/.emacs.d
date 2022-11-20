;; --- basic
(with-eval-after-load 'org
  (progn
    (setq org-directory "~/.gclrc/org/")

    ;; -- basic
    (setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300)
          org-html-doctype "html5")

    ;; -- 使用 “+” 来切换列表风格，- -> 1. -> a. ...
    (evil-define-key 'normal org-mode-map
      "+" #'org-cycle-list-bullet)

    ;; -- keywords
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

    ;; -- paint
    (setq org-plantuml-jar-path "~/.gclrc/plantuml.jar")
    (setq org-ditaa-jar-path "~/.gclrc/ditaa.jar")


    ;; -- emphasis 设置
    (setq org-emphasis-alist
          '(("*" my-org-emphasis-bold)
            ("/" my-org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))
    (defface my-org-emphasis-bold
      '((default :inherit bold)
        (((class color) (min-colors 88) (background light))
         :foreground "#a60000")
        (((class color) (min-colors 88) (background dark))
         :foreground "#ff8059"))
      "My bold emphasis for Org.")

    (defface my-org-emphasis-italic
      '((default :inherit italic)
        (((class color) (min-colors 55) (background light))
         :foreground "#972500")
        (((class color) (min-colors 55) (background dark))
         :foreground "#ef8b50"))
      "My italic emphasis for Org.")

    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    ;; ------------- end --------------
    )
  )

;; --- super agenda
(use-package org-super-agenda
  :config
  (defvar org-agenda-dir ""
    "gtd org files location")

  (defvar deft-dir ""
    "deft org files locaiton")

  (setq org-agenda-dir "~/.gclrc/org/")
  (setq deft-dir  "~/.gclrc/org/")
  (setq org-agenda-log-mode-items '(clock closed state))

  (setq org-agenda-inhibit-startup t) ;; ~50x speedup
  (setq org-agenda-span 'day)
  (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-done t)
  (setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note))
  )

;; --- svg-tag
(use-package svg-tag-mode
  :after org
  :hook (org-mode . svg-tag-mode)
  :config
  (defun mk/svg-checkbox-empty ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-filled ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
                   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-toggle ()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
             (end-pos (line-end-position))
             (text (buffer-substring-no-properties start-pos end-pos))
             (case-fold-search t)       ; Let X and x be the same in search
             )
        (beginning-of-line)
        (cond ((string-match-p "\\[X\\]" text)
               (progn
                 (re-search-forward "\\[X\\]" end-pos)
                 (replace-match "[ ]")))
              ((string-match-p "\\[ \\]" text)
               (progn
                 (search-forward "[ ]" end-pos)
                 (replace-match "[X]")))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-tags
        `(
          ;; -- Number
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ;; -- Task priority
          ("\\[#[A-Z]\\]" . ((lambda (tag)
                               (svg-tag-make tag :face 'org-priority
                                             :beg 2 :end -1 :margin 0))))
          ;; -- Tags
          ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2))))
          ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                         (svg-tag-make tag :beg 2 :end -1))))

          ;; -- Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; -- Checkbox
          ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
                        (lambda () (interactive) (mk/svg-checkbox-toggle))
                        "Click to toggle."))
          ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
                                 (lambda () (interactive) (mk/svg-checkbox-toggle))
                                 "Click to toggle."))

          ;; -- Date: Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; -- Date: Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; Keywords
          ;; ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-done :margin 0 :radius 5))))

          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :inverse t :margin 0 :crop-right t))))
          ("TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-tag :inverse t :margin 0 :crop-right t))))
          ("WORK\\b" . ((lambda (tag) (svg-tag-make "WORK" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ))
  )


;; --- org-block
(with-eval-after-load 'org
  (progn
    (require 'org-tempo)
    (require 'org-src)
    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
    (setq org-confirm-babel-evaluate nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-preserve-indentation t
          ;; or current-window
          org-src-window-setup 'other-window)


    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
    (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("go" . "src go"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
    (add-to-list 'org-structure-template-alist '("json" . "src json"))
    ))

;; --- evil-org
;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () org-superstar-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; --- appear
(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (defun org-apperance-evil-hack ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook #'org-apperance-evil-hack)
  )

;; --- superstar
(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-special-todo-items t)
  )

;; --- mac link
(use-package org-mac-link)

;; --- fragtog
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; --- org-capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
        ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
         "* %?\n  %i\n %U"
         :empty-lines 1)
        ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
        ("s" "Slipbox" entry  (file "inbox.org")
         "* %?\n")
        ("S" "Code Snippet" entry
         (file org-agenda-file-code-snippet)
         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ("w" "work" entry (file+headline org-agenda-file-work "Work")
         "* TODO [#A] %?\n  %i\n %U"
         :empty-lines 1)
        ("x" "Web Collections" entry
         (file+headline org-agenda-file-note "Web")
         "* %U %:annotation\n\n%:initial\n\n%?")
        ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
         "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
         :empty-lines 1)
        ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
         "* TODO [#C] %?\n  %i\n %a \n %U"
         :empty-lines 1)
        ("j" "Journal Entry"
         entry (file+datetree org-agenda-file-journal)
         "* %?"
         :empty-lines 1)))

;; --- org-download
(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :config
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "~/.img/tmp/"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
  )

;; --- org-special-block-extras
(use-package org-special-block-extras
  :ensure t
  :after org
  :hook (org-mode . org-special-block-extras-mode)
  ;; All relevant Lisp functions are prefixed ‘o-’; e.g., `o-docs-insert'.
  :custom
  (o-docs-libraries
   '("~/.posts/documentations.org")
   "The places where I keep my ‘#+documentation’"))

;; --- org-roam
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename user-blog-posts))
  (org-roam-dailies-directory "daily/")
  :config
  (defun gcl/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  )

;; --- org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
	org-roam-ui-browser-function #'browse-url
	))

;; --- org-roam bindings
(general-define-key
 "C-c n n" 'org-roam-node-find
 "C-c n g" 'org-roam-graph
 "C-c n r" 'org-roam-node-random
 "C-c n c" 'org-roam-capture
 "C-c n d" 'org-roam-dailies-capture-today
 "C-c n u" 'org-roam-ui-open
 "C-c n f" 'consult-org-roam-file-find
 "C-c n b" 'consult-org-roam-backlinks
 "C-c n l" 'consult-org-roam-forward-links
 "C-c n s" 'consult-org-roam-search
 )

(general-def org-mode-map
  "C-c n i" 'org-roam-node-insert
  "C-c n o" 'org-id-get-create
  "C-c n t" 'org-roam-tag-add
  ;; "C-c n e" 'org-roam-extract-subtree
  "C-c n a" 'org-roam-alias-add
  "C-c n ," 'org-roam-buffer-toggle)

;; --- org bindings
(evil-define-key 'normal org-mode-map
  "tt" 'org-todo
  "tp" 'org-priority
  "td" 'org-deadline
  "tc" 'org-capture
  "tl" 'org-store-link
  "tn" 'org-add-note
  "t," 'org-toggle-checkbox

  ;; clock
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "cg" 'org-clock-goto
  "cx" 'org-clock-cancel
  "ck" 'org-clock-timestamps-up
  "cj" 'org-clock-timestamps-down
  )

(provide 'init-org)
