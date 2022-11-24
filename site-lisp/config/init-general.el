(use-package general)

;; --- fn
(general-define-key
 ;; "<f2>" 'restart-emacs
 )

(general-create-definer global-leader
  :keymaps 'override
  :states '(emacs normal hybrid motion visual operator)
  :prefix ","
  "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

;; --- ,
(global-leader
  ;; "c" 'blamer-show-posframe-commit-info
  "," 'hydra-smerge/body
  ;; "l" 'hydra-lsp-mode/body
  "l" 'hydra-lsp-bridge/body
  "r" 'hydra-roam/body
  "s" 'hydra-search/body
  "t" 'treemacs
  )

;; --- SPC
(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(global-definer
  "TAB" 'projectile-persp-switch-project
  "SPC" 'execute-extended-command
  "0" 'winum-select-window-0
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "," 'delete-window
  "." 'kill-this-buffer
  ";" 'kill-other-window-buffer
  "x" 'scratch-buffer
  "`" 'multi-vterm-project
  )

;; 可以定制 SPC <key1> <key2> ...
(defmacro +general-global-menu! (name infix-key &rest body)
  "Create a definer named +general-global-NAME wrapping global-definer.
    Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     (general-create-definer ,(intern (concat "+general-global-" name))
       :wrapping global-definer
       :prefix-map ',(intern (concat "+general-global-" name "-map"))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,name))
     (,(intern (concat "+general-global-" name))
      ,@body)))

;; --- SPC a 应用类
(+general-global-menu! "apps" "a"
  "a" 'org-agenda
  "c" 'agenda
  "l" 'app-launcher-run-app
  )

;; --- SPC b Buffer 相关
(+general-global-menu! "buffer" "b"
  "b" 'consult-buffer
  "o" 'consult-buffer-other-window
  "p" 'previous-buffer
  "n" 'next-buffer
  "k" 'kill-buffer
  "d" 'kill-current-buffer
  "i" 'ibuffer
  "r" 'crux-rename-buffer-and-file
  "x" 'crux-kill-other-buffers
  "f" 'gcl/open-current-directory
  )

;; --- SPC e 异常
(+general-global-menu! "errors" "e"
  "e" 'consult-lsp-diagnostics
  )

;; --- SPC f 文件相关
(+general-global-menu! "files" "f"
  "o" 'crux-open-with
  "p" 'consult-find
  "f" 'find-file
  "d" 'crux-delete-file-and-buffer
  "r" 'crux-rename-file-and-buffer
  )

; --- SPC l 加载或链接相关
(+general-global-menu! "load&link" "l"
  ;; test: https://blog.cheng92.com
  "o" 'link-hint-open-link
  "c" 'link-hint-copy-link
  )

;; --- SPC p 项目相关
(+general-global-menu! "projects" "p"
  "p" 'consult-projectile-switch-project
  "f" 'consult-projectile-find-file
  "d" 'consult-projectile-find-dir
  "b" 'consult-projectile-switch-to-buffer
  "B" 'consult-project-buffer
  )

;; --- SPC q 查询相关
(+general-global-menu! "query" "q"
  ;; "r" 'restart-emacs
  )

;; --- SPC s 搜索相关
(+general-global-menu! "search" "s"
  "p" 'consult-ripgrep
  "v" 'consult-lsp-symbols
  )

;; --- SPC t treemacs
(+general-global-menu! "treemacs" "t"
  "1" 'treemacs-delete-other-windows
  "," 'treemacs
  "d" 'treemacs-select-directory
  "b" 'treemacs-bookmark
  "f" 'treemacs-find-file
  "t" 'treemacs-find-tag
  )

;; --- SPC w 窗口相关
(+general-global-menu! "window" "w"
  "," 'delete-window
  "-" 'split-window-below
  "v" 'split-window-right
  "m" 'delete-other-windows
  "h" 'evil-window-left
  "l" 'evil-window-right
  "j" 'evil-window-down
  "k" 'evil-window-up)

;; --- Control 组合键(C)
(general-define-key
 ;; "C-;" 'embark-act
 "C-=" 'er/expand-region
 ;; "C-a" 'crux-move-beginning-of-line
 "C-r" 'crux-rename-buffer-and-file
 "C-j" 'emmet-expand-yas
 "C-s" 'consult-line
 "C-'" 'toggle-quotes-plus
 "C-`" 'vterm-toggle
 "C-w" 'evil-delete-backward-word
 "C-p" 'previous-line
 "C-S-h" 'buf-move-left
 "C-S-l" 'buf-move-right
 "C-S-j" 'buf-move-down
 "C-S-k" 'buf-move-up
 "C-S-o" 'duplicate-line-or-region-above
 "C-S-n" 'duplicate-line-or-region-below

 ;; C-c
 ;; 1 + 2 + 3

 "C-c =" 'math-at-point
 "C-c b" 'consult-bookmark
 "C-c h" 'consult-history
 "C-c o" 'consult-outline
 ;; "C-c c" 'copy-buffer-file-name-as-kill
 "C-c e" 'consult-flycheck
 "C-c r" 'vr/replace
 "C-c q" 'vr/query-replace
 "C-c m" 'vr/mc-mark
 "C-c u" 'uuidgen
 "C-c f" 'devdocs-lookup

 ;; C-c i, insert
 "C-c i u" 'org-mac-link-get-link
 "C-c i s" 'yas-insert-snippet
 "C-c i y" 'consult-yasnippet
 "C-c i t" 'gcl/insert-current-time
 "C-c i d" 'gcl/insert-standard-date
 "C-c i f" 'js-doc-insert-function-doc
 "C-c i F" 'js-doc-insert-file-doc

 ;; C-c g, git
 ;; ...

 ;; C-c t, toggle
 "C-c t t" 'treemacs
 "C-c t m" 'git-timemachine-toggle

 ;; yas & fanyi
 "C-c y y" 'fanyi-dwim2
 "C-c y n" 'yas-new-snippet
 "C-c y r" 'yas-reload-all
 "C-c y v" 'yas-visit-snippet-file
 "C-c y V" 'consult-yasnippet-visit-snippet-file
 "C-c y s" 'sdcv-search-pointer+
 "C-c y i" 'sdcv-search-input+
 )

;; --- Command 组合键(s)
(general-define-key
 "<s-backspace>" 'crux-kill-line-backwards
 "<s-left>" 'windmove-left
 "<s-right>" 'windmove-right
 "<s-down>" 'windmove-down
 "<s-up>" 'windmove-up
 "s-," 'bury-buffer
 "s-." 'unbury-buffer
 "s-<" 'watch-other-window-up-line
 "s->" 'watch-other-window-down-line
 ;; "s-0" 'sp-splice-sexp
 "s-p" 'move-text-up
 "s-n" 'move-text-down
 "s-m" 'toggle-input-method
 "s-o" 'toggle-one-window
 "s-R" 're-builder
 ;; "s-i" 'gcl/string-inflection-cycle-auto
 "s-d" 'consult-dir
 "s-F" 'format-all-buffer
 ;; "s-h" 'gcl/urls/body
 "s-`" 'vterm-toggle
 "s-'" 'vertico-repeat

 ;; s-g
 "s-g" nil
 "s-g o" 'consult-outline
 "s-g m" 'consult-mark
 "s-g k" 'consult-global-mark
 "s-g i" 'consult-yasnippet
 )

;; --- Option/Alt 组合键(M)
(general-define-key
 ;; M, Option/Alt
 "M-0" 'treemacs
 "M-s" 'symbol-overlay-put
 "M-n" 'symbol-overlay-switch-forward
 "M-p" 'symbol-overlay-switch-backward
 "M-c" 'symbol-overlay-remove-all
 ;; "M-*" 'tempel-insert
 ;; "M-+" 'tempel-complete
 "M-'" 'consult-register-store
 "M-#" 'consult-register-load
 "M-;" 'evilnc-comment-or-uncomment-lines
 ;; "M-e" 'emojify-insert-emoji
 "M-d" 'dash-at-point
 ;; "M-j" 'rime-inline-ascii
 "M-i" 'consult-imenu
 "M-m" 'blamer-show-posframe-commit-info
 )

(provide 'init-general)
