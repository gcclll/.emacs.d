(defun zilongshanren/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;; (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  )

(add-hook 'org-mode-hook #'zilongshanren/org-ispell)

;; ;; 粗体字高亮，带颜色
;; (defface hi-red-b '((t (:foreground "#e50062"))) t)
;; (defun org-bold-highlight ()
;;   (interactive)
;;   (hi-lock-mode)
;;   (highlight-regexp "[ \\t]\\(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*" 'hi-red-b))
;; (add-hook 'org-mode-hook 'org-bold-highlight)
