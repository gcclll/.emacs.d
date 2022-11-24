
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)
(require 'acm-backend-tailwind)

(global-lsp-bridge-mode)
;; (add-hook 'emacs-lisp-mode-hook 'lsp-bridge-mode)
;; (add-hook 'sh-mode-hook 'lsp-bridge-mode)
;; (add-hook 'python-mode-hook 'lsp-bridge-mode)

(setq acm-enable-tabnine nil)
;; (lsp-bridge-enable-auto-import)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-find-def-return))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

;; --- deno
(setq lsp-bridge-get-single-lang-server-by-project
      (lambda (project-path filepath)
        ;; If typescript first line include deno.land, then use Deno LSP server.
        (save-excursion
          (when (string-equal (file-name-extension filepath) "ts")
            (dolist (buf (buffer-list))
              (when (string-equal (buffer-file-name buf) filepath)
		(with-current-buffer buf
                  (goto-char (point-min))
                  (when (string-match-p (regexp-quote "from \"https://deno.land") (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                    (return "deno")))))))))

;; 打开日志，开发者才需要
(setq lsp-bridge-enable-log nil)

(general-define-key
 "C-5" 'lsp-bridge-diagnostic-list
 "C-6" 'lsp-bridge-lookup-documentation
 "C-7" 'lsp-bridge-jump-back
 "C-8" 'lsp-bridge-jump
 "C-9" 'lsp-bridge-find-references
 "C-0" 'lsp-bridge-rename)

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "gd") 'lsp-bridge-jump)
  (evil-global-set-key 'normal (kbd "gb") 'lsp-bridge-jump-back)
  (evil-global-set-key 'normal (kbd "gf") 'lsp-bridge-find-references)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
  )

(provide 'init-lsp-bridge)
