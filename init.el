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

;; time date
(defun gcl/insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun gcl/insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun gcl/insert-current-time ()
  "Insert current time, like hh:mm:ss."
  (interactive)
  (insert (format-time-string "%T")))

(defun gcl/open-current-directory ()
  (interactive)
  (consult-file-externally default-directory))

(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

(defun gcl/get-frame->selected-window ()
  "Returns a list of pairs of (frame selected-window)"
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                      (-map (lambda (f)
                              (select-frame f t)
                              (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(eval-when-compile
  (require 'cl))
(defun gcl/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but desire to keep your current window focused."
  ;; Note that we must preserve the selected window of every frame, because the function being executed may
  ;; change the focused frame, even if the current frame is in focus.
  (lexical-let* ((original-frame (selected-frame))
                 (frames->windows (gcl/get-frame->selected-window))
                 (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (first x) t)
                             (select-window (second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))

  (defun reload-init-file ()
    "Reload init file with <f5>."
    (interactive)
    (load-file "~/.emacs.d/init.el"))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (and (file-readable-p custom-file) (load custom-file))

  (defun gcl/open-init-file()
    (interactive)
    (find-file (expand-file-name "README.org" user-emacs-directory)))

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

(setq blog-admin-dir "~/github/mine/blog.cheng92.com/")
(setq user-full-name "Lee ZhiCheng"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://blog.cheng92.com"
      user-github-dir "~/github/mine/"
      user-web-dir "~/github/mine/gcl-web-system/"
      user-blog-dir (concat user-web-dir "apps/blog/")
      user-blog-public-dir (concat user-blog-dir "public/")
      user-blog-posts (concat user-blog-dir "/public/posts/")
      user-dot-dir "~/.gclrc/"
      user-dot-bin-dir "~/.gclrc/bin/"
      )

  ;; 启动全屏
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

  ;; 选中粘贴时能覆盖选中的内容
  (delete-selection-mode 1)
  ;; 高亮当前行
  (global-hl-line-mode 1)
  ;; 指针不闪动。
  (blink-cursor-mode -1)
  ;; 有些功能需要用到，比如：折叠等等
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  ;; 选择是或否是用 y/n
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; 默认显示 80 列就换行
  (setq default-fill-column 80)
  ;; 用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
  (setq kill-ring-max 1024)
  ;; 设置的 mark ring 容量
  (setq mark-ring-max 1024)
  ;; 设置执行表达式的长度没有限制
  (setq eval-expression-print-length nil)
  ;; 设置执行表达式的深度没有限制
  (setq eval-expression-print-level nil)
  ;; 设置最大的全局标记容量
  (setq global-mark-ring-max 1024)
  ;; minibuffer 递归调用命令
  (setq enable-recursive-minibuffers t)
  ;; 删除minibuffer的重复历史
  (setq history-delete-duplicates t)
  ;; 显示消息超时的时间
  (setq minibuffer-message-timeout 1)
  ;; 自动更新 buffer
  (setq auto-revert-mode 1)
  ;; 括号匹配显示但不是烦人的跳到另一个括号。
  (setq show-paren-style 'parentheses)
  ;; 当插入右括号时显示匹配的左括号
  (setq blink-matching-paren t)
  ;; 不自动添加换行符到末尾, 有些情况会出现错误
  (setq require-final-newline nil)
  ;; 比较窗口设置在同一个 frame 里
  (setq ediff-window-setup-function (quote ediff-setup-windows-plain))
  ;; 设置传送文件默认的方法
  (setq tramp-default-method "ssh")
  ;; 禁止显示鼠标指针
  (setq void-text-area-pointer nil)
  ;; 当出现异常时弹出三角警告
  (setq visible-bell t)
  ;; 显示行尾空格
  (setq show-trailing-whitespace t)
  (setq create-lockfiles nil)

  ;; --- 关闭启动消息。
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)
  (setq initial-scratch-message nil)
  ;; 改变 *scratch* buffer 的模式
  (setq initial-major-mode 'emacs-lisp-mode)
  (setq initial-buffer-choice t)
  ;; *scratch* buffer 初始显示的内容
  (setq initial-scratch-message "\
  ;; This buffer is for notes you don't want to save, and for Ruby code.
  ;; If you want to create a file, visit that file with C-x C-f,
  ;; then enter the text in that file's own buffer.")

  (defun max-gc-limit ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun reset-gc-limit ()
    (setq gc-cons-threshold 800000))

  ;; 设置垃圾回收限制
  (add-hook 'minibuffer-setup-hook #'max-gc-limit)
  (add-hook 'minibuffer-exit-hook #'reset-gc-limit)
  (setq-default bidi-display-reordering nil)

  ;; 加速启动
  (setq auto-mode-case-fold nil)
  ;; 加快快捷键提示的速度
  (setq echo-keystrokes 0.1)

  ;; 提升 IO 性能。
  (setq process-adaptive-read-buffering nil)
  ;; 增加单次读取进程输出的数据量（缺省 4KB) 。
  (setq read-process-output-max (* 1024 1024))

  ;; 缩短 fontify 时间。
  (setq jit-lock-defer-time nil)
  (setq jit-lock-context-time 0.1)
  ;; 更积极的 fontify 。
  (setq fast-but-imprecise-scrolling nil)
  (setq redisplay-skip-fontification-on-input nil)

  ;; 缩短更新 screen 的时间。
  (setq idle-update-delay 0.1)

  ;; 使用字体缓存，避免卡顿。
  (setq inhibit-compacting-font-caches t)
  ;; 使用更瘦字体。
  (setq ns-use-thin-smoothing t)
  ;; 一次滚动一行，避免窗口跳动。
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (setq mouse-wheel-scroll-amount-horizontal 1)
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed nil)

  (defconst 1mb 1048576)
  (defconst 20mb 20971520)
  (defconst 30mb 31457280)
  (defconst 50mb 52428800)

  ;; lsp-mode's performance suggest
  (setq read-process-output-max (* 3 1mb))

(use-package so-long
  :straight (:host github :repo "hlissner/emacs-so-long")
  :config
  (add-hook 'after-init-hook #'global-so-long-mode)
  (setq so-long-threshold 40000))

(set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono")

(use-package font-lock+
  :straight (:host github :repo "emacsmirror/font-lock-plus"))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode)))

;; -- header line
(set-face-attribute 'header-line nil
                    :foreground (face-attribute 'mode-line :foreground)
                    :background (face-attribute 'mode-line :background)
                    ;; height of mode-line is also unspecified, so we set it directly.
                    :height 150
                    :box (face-attribute 'mode-line :box))

;; -- awesome tray
(use-package awesome-tray
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :config
  (setq awesome-tray-mode-line-height 0.1)
  ;; (setq-default awesome-tray-mode-line-default-height 0.1)
  (setq awesome-tray-mode-line-active-color "#EC4899")
  (setq awesome-tray-mode-line-inactive-color "#959eb1")
  (setq awesome-tray-active-modules '(
				      ;; "location"
				      "pdf-view-page"
				      "date"
				      "file-path"
				      "buffer-name"
				      "mode-name"
				      "battery"
				      "git"
				      "input-method"
				      "evil"
				      ;; "flymake"
				      "belong"
				      "anzu"
				      ;; "github"
				      ))
  (setq awesome-tray-date-format "%d/%H:%M:%S")

  (awesome-tray-mode 1))

;; -- highlight-parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  )

;; -- 丰富括号
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package marginalia
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'right)
    :init
    (marginalia-mode))

(use-package all-the-icons-completion
    :after (marginalia all-the-icons)
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :init
    (all-the-icons-completion-mode))

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*")
		                 :includes (vertico-indexed
				                        vertico-flat
				                        vertico-grid
				                        vertico-mouse
				                        vertico-quick
				                        vertico-buffer
				                        vertico-repeat
				                        vertico-reverse
				                        vertico-directory
				                        vertico-multiform
				                        vertico-unobtrusive
				                        ))
  ;; Make sure vertico state is saved for `vertico-repeat'
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.1)
  ;;(setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  )

(use-package evil
  :ensure t
  :init
  (evil-mode)
  :config
  ;; 退出编辑模式后光标留在原地
  (setq evil-move-cursor-back nil)
  ;; 让回车，TAB，空格键保持原来的功能
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-motion-state-map (kbd "SPC") nil))
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; --- 解绑一些按键
    (evil-global-set-key 'normal (kbd "c") nil)

    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (setq-default evil-ex-search-persistent-highlight nil)

    (define-key evil-motion-state-map (kbd "0") 'evil-end-of-line)

    (evil-global-set-key 'normal "f" 'evil-avy-goto-char)
    (evil-global-set-key 'normal "w" 'evil-avy-goto-word-or-subword-1)
    (evil-global-set-key 'normal "s" 'evil-avy-goto-line)
    (evil-global-set-key 'motion "-" 'org-decrease-number-at-point)
    (evil-global-set-key 'motion "+" 'org-increase-number-at-point)

    (evil-global-set-key 'normal (kbd "gd") 'xref-find-definitions)
    (evil-global-set-key 'normal (kbd "gb") 'xref-pop-marker-stack)
    (evil-global-set-key 'normal (kbd "gc") 'show-commit-and-preserve-window)

    (evil-global-set-key 'normal (kbd "cc") 'evilnc-copy-and-comment-lines)
    )
  )

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (setq-default evil-surround-pairs-alist
		            '((?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))

                  (?\) . ("( " . " )"))
                  (?\] . ("[ " . " ]"))
                  (?\} . ("{ " . " }"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))

                  (?\/ . ("/* " . " */"))

                  ;; Single-quoted strings
                  (?\' . ("'" . "'"))

                  ;; Emacs-style quotes
                  (?\` . ("`" . "'"))
		              ;; javascript
                  (?\` . ("`" . "`"))

                  ;; Python multi-line strings
                  (?d . ("\"\"\"" . "\"\"\""))
                  (?D . ("'''" . "'''"))

                  (?t . evil-surround-read-tag)
                  (?< . evil-surround-read-tag)
                  (?f . evil-surround-function)))
  (global-evil-surround-mode 1))

;; (evil-set-initial-state 'color-rg-mode 'emacs)
;; (evil-set-initial-state 'multi-vterm-mode 'emacs)
;; (evil-set-initial-state 'vterm-mode 'emacs)
;; (evil-set-initial-state 'magit-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'magit-branch-manager-mode 'emacs)

(use-package general)

(general-define-key
 ;; "<f2>" 'restart-emacs
 )

(general-create-definer global-leader
  :keymaps 'override
  :states '(emacs normal hybrid motion visual operator)
  :prefix ","
  "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(global-leader
  ;; "c" 'blamer-show-posframe-commit-info
  "," 'hydra-smerge/body
  "l" 'hydra-lsp/body
  "r" 'hydra-roam/body
  "t" 'treemacs
  )

  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (global-definer
   "TAB" 'projectile-persp-switch-project
   "SPC" 'execute-extended-command
   "0" 'select-window-0
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   "," 'delete-window
   "." 'kill-this-buffer
   ";" 'kill-other-window-buffer
   "x" 'switch-to-scratch-buffer
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

  (+general-global-menu! "apps" "a"
			 "a" 'org-agenda
			 "c" 'agenda
       ;; "l" 'app-launcher-run-app
       )

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

(+general-global-menu! "errors" "e"
  "e" 'consult-lsp-diagnostics
	)

  (+general-global-menu! "files" "f"
			 "o" 'crux-open-with
			 "p" 'consult-find
			 "f" 'find-file
			 "d" 'crux-delete-file-and-buffer
			 "r" 'crux-rename-file-and-buffer
			 )

  (+general-global-menu! "load&link" "l"
			 ;; test: https://blog.cheng92.com
			 "o" 'link-hint-open-link
			 "c" 'link-hint-copy-link
			 )

  (+general-global-menu! "projects" "p"
			 "p" 'consult-projectile-switch-project
			 "f" 'consult-projectile-find-file
			 "d" 'consult-projectile-find-dir
			 "b" 'consult-projectile-switch-to-buffer
			 "B" 'consult-project-buffer
			 )

  (+general-global-menu! "query" "q"
			 ;; "r" 'restart-emacs
			 )

(+general-global-menu! "search" "s"
  "p" 'consult-ripgrep
  "v" 'consult-lsp-symbols
  )

(+general-global-menu! "treemacs" "t"
  "1" 'treemacs-delete-other-windows
  "," 'treemacs
  "d" 'treemacs-select-directory
  "b" 'treemacs-bookmark
  "f" 'treemacs-find-file
  "t" 'treemacs-find-tag
  )

  (+general-global-menu! "window" "w"
			 "," 'delete-window
			 "-" 'split-window-below
			 "v" 'split-window-right
			 "m" 'delete-other-windows
			 "h" 'evil-window-left
			 "l" 'evil-window-right
			 "j" 'evil-window-down
			 "k" 'evil-window-up)

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
   ;; C-c f  -> hydra-lsp/body
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
   "C-c t" 'treemacs

   ;; C-c i, insert
   "C-c i u" 'org-mac-link-get-link
   "C-c i s" 'yas-insert-snippet
   "C-c i y" 'consult-yasnippet
   "C-c i t" 'gcl/insert-current-time
   "C-c i d" 'gcl/insert-standard-date

   ;; C-c g, git
   ;; ...

   ;; yas & fanyi
   "C-c y y" 'fanyi-dwim2
   "C-c y n" 'yas-new-snippet
   "C-c y r" 'yas-reload-all
   "C-c y v" 'yas-visit-snippet-file
   "C-c y V" 'consult-yasnippet-visit-snippet-file
   )

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

(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(define-abbrev-table
  'global-abbrev-table '(
			 ;; signature
			 ("8zc" "lizhicheng")
			 ("8zj" "李志诚")
			 ("8lp" "范婷婷")
			 ;; Microsoft
			 ("8ms" "Microsoft")
			 ("8em" "gccll.love@gmail.com")
			 ("8bl" "https://blog.cheng92.com")
			 ))

(use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
	   ("C-M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package fancy-dabbrev
  :ensure t
  :config
  (global-fancy-dabbrev-mode)
  ;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
  ;; choice, here "TAB" and "Shift+TAB":
  (global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
  (global-set-key (kbd "M-?") 'fancy-dabbrev-backward)

  ;; If you want TAB to indent the line like it usually does when the cursor
  ;; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
  ;; instead of `fancy-dabbrev-expand`:
  ;; (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
  ;; (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
  ;; Let dabbrev searches ignore case and expansions preserve case:
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))
(use-package yasnippet-snippets
  :defer t
  :after yasnippet)
(yas-global-mode 1)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; 有变化时自动保存
(setq bookmark-save-flag 1)

(use-package expand-region)

(use-package smartparens
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
	:map smartparens-strict-mode-map
        ("C-M-<backspace>" . sp-backward-unwrap-sexp)
        ("C-M-d" . sp-unwrap-sexp))
  :hook
  ((prog-mode . smartparens-mode)
   ;; (smartparens-mode . smartparens-strict-mode)
   )
  :config (require 'smartparens-config))

(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package symbol-overlay)

(use-package move-text)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package toggle-quotes-plus
  :straight (:host github :repo "jcs-elpa/toggle-quotes-plus")
  :config
  (setq toggle-quotes-plus-chars '("\""
                                 "'"
                                 "`")))

(use-package hydra)

(defhydra hydra-roam (:exit t :columns 3)
  "org-roam
-------------------------------------------------------------------------------------
"
  ("," org-roam-buffer-toggle "Toggle Buffer")
  ("a" org-roam-alias-add "Add Alias")
  ("b" consult-org-roam-backlinks "Backward Links")
  ("c" org-roam-capture "Capture")
  ("C" org-id-get-create "Create ID")
  ("d" org-roam-dailies-capture-today "Capture Dailies")
  ("f" consult-org-roam-file-find "Find File")
  ("g" org-roam-graph "Graph")
  ("i" org-roam-node-insert "Insert Node")
  ("l" consult-org-roam-forward-links "Forward Links")
  ("n" org-roam-node-find "Find Node")
  ("r" org-roam-node-random "Random Node")
  ("s" consult-org-roam-search "Search")
  ("t" org-roam-tag-add "Add Tag")
  ("u" org-roam-ui-open "Roam UI")
  ("q" nil "Quit")
  )

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" format-all-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(defhydra gcl/smerge (:color red :hint nil)
  "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("c" smerge-keep-current)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("e" smerge-ediff)
  ("j" previous-line)
  ("k" forward-line)
  ("r" smerge-refine)
  ("u" undo)
  ("q" nil :exit t))

(use-package crux)

(use-package fanyi
  :config
  (custom-set-variables
   '(fanyi-providers '(fanyi-haici-provider
                       fanyi-youdao-thesaurus-provider
                       fanyi-etymon-provider
                       fanyi-longman-provider
                       fanyi-libre-provider)))

  ;; 还要自动选择翻译内容 buffer
  (setq fanyi-auto-select nil))

(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package math-at-point
  :straight (:host github :repo "shankar2k/math-at-point"))

(use-package uuidgen)

(require 'dired)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "open")
        ("\\.docx\\'" "open")
        ("\\.\\(?:djvu\\|eps\\)\\'" "open")
        ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
        ("\\.\\(?:xcf\\)\\'" "open")
        ("\\.csv\\'" "open")
        ("\\.tex\\'" "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
         "open")
        ("\\.\\(?:mp3\\|flac\\)\\'" "open")
        ("\\.html?\\'" "open")
        ("\\.md\\'" "open")))

(use-package diredfl
  :hook ((dired-mode . diredfl-mode)))

(use-package dash-at-point)

(use-package devdocs)

(use-package httprepl)

(setenv "NODE_PATH" "/usr/local/lib/node_modules")

(use-package editorconfig
  :config
  (editorconfig-mode 1))

  ;; Optionally use the `orderless' completion style.
  (use-package orderless
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(orderless-fast)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion)))))


    :config
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
	   `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-literal orderless-regexp)))
    )

  (use-package corfu
    :after orderless
    ;; Optional customizations
    :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    (corfu-quit-at-boundary nil)     ;; Automatically quit at word boundary
    (corfu-quit-no-match t)        ;; Automatically quit if there is no match
    (corfu-auto-delay 0)
    ;; 输入两个字符开始实例
    (corfu-auto-prefix 2)
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    (corfu-preview-current nil)    ;; Disable current candidate preview
    (corfu-preselect-first t)    ;; Enable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin
    ;; (corfu-min-width 80)
    (corfu-max-width 80)
    :bind
    (:map corfu-map
	  ("C-j" . corfu-next)
	  ("C-k" . corfu-previous)
	  ("<escape>" . corfu-quit)
	  ;; ("M-l" . corfu-show-location)
	  ;; ("M-d" . corfu-show-documentation)
	  )
    :init
    (global-corfu-mode)
    :config
    (defun corfu-enable-always-in-minibuffer ()
      "Enable Corfu in the minibuffer if Vertico/Mct are not active."
      (unless (or (bound-and-true-p mct--active)
		  (bound-and-true-p vertico--input))
	;; (setq-local corfu-auto nil) Enable/disable auto completion
	(corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
    )

  (use-package kind-icon
    :after corfu
    :custom
    (kind-icon-use-icons t)
    (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
    (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
    (kind-icon-blend-frac 0.08)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
    )

  (use-package corfu-doc
    ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
    :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
    :after corfu
    :hook (corfu-mode . corfu-doc-mode)
    :bind
    (:map corfu-map
	  ("M-n" . corfu-doc-scroll-up)
	  ("M-p" . corfu-doc-scroll-down))
    :custom
    (corfu-doc-delay 0.5)
    (corfu-doc-max-width 70)
    (corfu-doc-max-height 20)

    ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
    ;; extra-safe that this is set when corfu-doc is loaded. I do not want
    ;; documentation shown in both the echo area and in the `corfu-doc' popup.
    (corfu-echo-documentation nil))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    (setq completion-cycle-threshold 2)
    (setq tab-always-indent 'complete))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((js2-mode . lsp)
	       (web-mode . lsp)
	       (typescript-mode . lsp)
	       (c-mode . lsp)
	       (c++-mode . lsp)
	       (python-mode . lsp)
	       (css-mode . lsp)
	       (lua-mode . lsp)
	       (shell-mode . lsp)
	       ;; if you want which-key integration
	       (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-completion-provider :none)
  ;; (lsp-auto-configure nil)
  :commands lsp
  :config
  (setq lsp-disabled-clients '(vls))
  ;; (setq lsp-enabled-clients '(lsp-volar))
  )


(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes
	      '(svelte-mode html-mode sgml-mode mhtml-mode web-mode css-mode js-mode))
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)
  )

(use-package lsp-volar
  :straight (:host github :repo "jadestrong/lsp-volar"))

(general-define-key
 "C-c l s" 'lsp-tailwindcss-rustywind
 "C-c l i" 'lsp-ui-imenu
 "C-c l d" 'lsp-ui-peek-find-definitions
 "C-c l r" 'lsp-ui-peek-find-references
 "C-c l a" 'lsp-organize-imports
 "C-c l e" 'lsp-treemacs-errors-list)

(use-package dap-mode
  :hook ((lsp-mode . dap-mode)
	       (lsp-mode . dap-ui-mode))
  :bind (:map dap-mode-map
		          ("C-c d d" . dap-debug)
		          ("C-c d h" . dap-hydra)
		          ("C-c d b" . dap-ui-breakpoints)
		          ("C-c d l" . dap-ui-locals)
		          ("C-c d r" . dap-ui-repl)))

(defun my/setup-js-mode ()
  (require 'dap-chrome)
  (setq tab-width 2)
  ;; 由于 lsp 已经提供了 diagnose 功能，故关闭 js2 自带的错误检查，防止干扰。
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors nil))

(use-package js2-mode
  :ensure t
  :after (lsp-mode dap-mode)
  :mode "\\.js\\'"
  :hook ((js2-mode . my/setup-js-mode)))

(use-package typescript-mode
  :ensure t
  :after (lsp-mode dap-mode)
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . my/setup-js-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package css-mode)
(use-package scss-mode)
(use-package emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  )

(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 0
   web-mode-script-padding 0
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing nil
   web-mode-enable-auto-indentation t
   web-mode-tag-auto-close-style 1
   web-mode-enable-current-element-highlight t)

  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
	          (cl-loop for pair in (cdr alist)
		                 unless (string-match-p "^[a-z-]" (cdr pair))
		                 collect (cons (car pair)
				                           (string-trim-right (cdr pair)
							                                        "\\(?:>\\|]\\|}\\)+\\'")))))
  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "vue"))
  )

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (equal "vue" (file-name-extension buffer-file-name))
;;               (let ((major-mode 'vue-mode))
;;                 (lsp)))))

(use-package python-mode)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . yaml-imenu-enable)))
(use-package yaml-imenu
  :after yaml-mode)

(use-package nxml-mode
  :straight (:type built-in))

(use-package php-mode)

(use-package sql
  :straight (:type built-in))
(use-package emacs-sql-indent
  :straight (:host github :repo "alex-hhh/emacs-sql-indent")
  :hook (sql-mode . sqlind-minor-mode))

(use-package lua-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package restclient)

(use-package format-all)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package pkg-info)
(use-package posframe)
(use-package flymake-posframe
  :straight (:host github :repo "Ladicle/flymake-posframe")
  :hook (flymake-mode . flymake-posframe-mode))

;;; --- 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.coffee$" . coffee-mode)
                    ("\\.iced$" . coffee-mode)
                    ("Cakefile" . coffee-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'" . haskell-mode)
                    ("\\.hs-boot\\'" . haskell-mode)
                    ("\\.chs\\'" . haskell-mode)
                    ("\\.l[hg]s\\'" . literate-haskell-mode)
                    ("\\.inc\\'" . asm-mode)
                    ("\\.max\\'" . maxima-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.a90\\'" . intel-hex-mode)
                    ("\\.hex\\'" . intel-hex-mode)
                    ("\\.py$" . python-mode)
                    ("SConstruct". python-mode)
                    ("\\.ml\\'" . tuareg-mode)
                    ("\\.mli\\'" . tuareg-mode)
                    ("\\.mly\\'" . tuareg-mode)
                    ("\\.mll\\'" . tuareg-mode)
                    ("\\.mlp\\'" . tuareg-mode)
                    ("\\.qml\\'" . qml-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.php\\'" . php-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.tsx$" . web-mode)
                    ("\\.ts$" . typescript-mode)
                    ("\\.js.erb\\'" . js2-mode)
                    ("\\.wxs$" . js2-mode)
                    ("\\.cjs$" . js2-mode)
                    ("\\.js$" . js2-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("\\.json$" . json-mode)
                    ("\\.coffee\\'" . coffee-mode)
                    ("\\.coffee.erb\\'" . coffee-mode)
                    ("\\.iced\\'" . coffee-mode)
                    ("Cakefile\\'" . coffee-mode)
                    ("\\.styl$" . sws-mode)
                    ("\\.jade" . jade-mode)
                    ("\\.go$" . go-mode)
                    ("\\.vala$" . vala-mode)
                    ("\\.vapi$" . vala-mode)
                    ("\\.rs$" . rust-mode)
                    ("\\.pro$" . qmake-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.swift$" . swift-mode)
                    ("\\.l$" . flex-mode)
                    ("\\.y$" . bison-mode)
                    ("\\.pdf$" . pdf-view-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.ll$" . llvm-mode)
                    ("\\.bc$" . hexl-mode)
                    ("\\.nim$" . nim-mode)
                    ("\\.nims$" . nim-mode)
                    ("\\.nimble$" . nim-mode)
                    ("\\.nim.cfg$" . nim-mode)
                    ("\\.exs$" . elixir-mode)
                    ("\\.clj$" . clojure-mode)
                    ("\\.svg$" . xml-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq c-basic-offset 4)

;; sh
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

(setq coffee-tab-width 2)
(setq javascript-indent-level 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq typescript-indent-offset 2)
(setq typescript-indent-level 2)

(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-sql-indent-offset 2)

(setq css-indent-offset 2)

(use-package magit
  :ensure t
  :config
  ;; 提交时候不显示提交细节
  (setq magit-commit-show-diff nil)
  ;; 没有焦点时候不刷新状态
  (setq magit-refresh-status-buffer nil)
  ;; 当前buffer打开magit
  (setq magit-display-buffer-function
	(lambda (buffer)
          (display-buffer buffer '(display-buffer-same-window))))
  (setq magit-ellipsis (get-byte 0 "."))
  ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  (defun show-commit-and-preserve-window ()
    (interactive)
    ;; NOTE(philc): I'm not sure why magit-show-commit needs to be called interactively, but just invoking it
    ;; directly gives an argument error.
    (gcl/preserve-selected-window (lambda ()
                                    (call-interactively 'magit-show-commit))))
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer buffer '(display-buffer-same-window))))
  ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  )

(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter " ● %s")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 120
                    :italic t)))
  :config
  ;; (global-blamer-mode 1)
  )

(use-package git-gutter
  :diminish
  :hook ((prog-mode . git-gutter-mode))
  :init
  (setq git-gutter:update-interval 0.5))

(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist
	       (cons "/.dockerignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
	       (cons "/.gitignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
               (cons "/.gitconfig\\'" 'gitconfig-mode))
  )

(use-package smerge
  :straight (:type built-in)
  :config
  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)
          (hydra-smerge/body))))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

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
      ))

  ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  (use-package evil-org
    :ensure t
    :after org
    :config
    (add-hook 'org-mode-hook (lambda () org-superstar-mode))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

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

(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-special-todo-items t)
  )



(use-package org-mac-link)

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

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

(use-package org-special-block-extras
  :ensure t
  :after org
  :hook (org-mode . org-special-block-extras-mode)
  ;; All relevant Lisp functions are prefixed ‘o-’; e.g., `o-docs-insert'.
  :custom
  (o-docs-libraries
   '("~/.posts/documentations.org")
   "The places where I keep my ‘#+documentation’"))

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

(use-package rime
  :config
  (setq rime-user-data-dir "~/github/mine/dot-emacs-rime")
  (setq rime-share-data-dir "~/Library/Rime")
  (setq rime-posframe-properties
	(list
         :internal-border-color "#000000"
         :internal-border-width 10
         ;; :font "TsangerJinKai03-6763 W05"
         :font "WenQuanYi Micro Hei Mono-16"
         ))
					;； “”
					;； 默认的前景色和背景色（仅posframe）
					;； 编码提示颜色 - rime-comment-face
					;； 编码的颜色   - rime-code-face
					;； 候选序号颜色 - rime-candidate-num-face
  (set-face-attribute 'rime-default-face nil :foreground "#eeeeee" :background "#000000")

  (setq default-input-method "rime"
	rime-show-candidate 'posframe

	;; rime-default-face "Red" ;默认的前景色和背景色（仅posframe）
	;; rime-code-face ;编码的颜色
	;; rime-candidate-num-face ;候选序号颜色
	;; rime-comment-face ;编码提示颜色

	;; posframe/popup/sidewindow 候选版式
	;; simple - 单行, horizontal - 水平，默认, vertical - 垂直
	rime-posframe-style 'vertical
	;; rime-popup-style
	;; rime-sidewindow-style
	)
  					;； 代码中自动使用英文
  (setq rime-disable-predicates
	'(rime-predicate-evil-mode-p ; evil 模式
          ;; 在英文字符串之后（必须为以字母开头的英文字符串）
          rime-predicate-after-alphabet-char-p
          ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          rime-predicate-prog-in-code-p
          ;; 激活 ace-window-mode
          rime-predicate-ace-window-p
          ;; 如果激活了一个 hydra keymap
          rime-predicate-hydra-p
          ;; 将要输入的为大写字母时
          rime-predicate-current-uppercase-letter-p
          ;; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-tex-math-or-command-p
          ))


;;;  support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings
	'("C-`" "S-<delete>" "C-f" "C-b" "C-n" "C-p" "C-g" "<return>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
					;； 临时强制转成英文模式，通过 rime-inline-ascii 恢复中文模式（什么鬼～～～）
					;；　(define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
  ;; (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)

  (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.50/include")
  ;; (setq-default rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  )

(use-package posframe)

(defun split-window--select-window (orig-func &rest args)
  "Switch to the other window after a `split-window'"
  (let ((cur-window (selected-window))
        (new-window (apply orig-func args)))
    (when (equal (window-buffer cur-window) (window-buffer new-window))
      (select-window new-window))
    new-window))
(advice-add 'split-window :around #'split-window--select-window)

(use-package duplicate-line
  :straight (:host github :repo "manateelazycat/duplicate-line"))

(use-package toggle-one-window
  :straight (:host github :repo "manateelazycat/toggle-one-window"))

(use-package watch-other-window
  :straight (:host github :repo "manateelazycat/watch-other-window"))

(use-package window-numbering
  :config
  (add-hook 'after-init-hook 'window-numbering-mode))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package vterm)
(use-package multi-vterm :ensure t)
(use-package vterm-toggle)
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local evil-insert-state-cursor 'box)
            (evil-insert-state)))
(define-key vterm-mode-map [return] #'vterm-send-return)
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
(define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
(define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                 (let ((buffer (get-buffer buffer-or-name)))
                   (with-current-buffer buffer
                     (or (equal major-mode 'vterm-mode)
                         (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-at-bottom)
               ;;(display-buffer-reuse-window display-buffer-in-direction)
               ;;display-buffer-in-direction/direction/dedicated is added in emacs27
               ;;(direction . bottom)
               ;;(dedicated . t) ;dedicated is supported in emacs27
               (reusable-frames . visible)
               (window-height . 0.3)))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)

  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine baidu "https://www.baidu.com/s?wd=%s"
	     :keybinding "b")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine qwant
    "https://www.qwant.com/?q=%s"
    :docstring "什么都能搜到哦~~😍😍"
    :keybinding "q")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "数学搜索引擎，公式，坐标图等。"
    :keybinding "w")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "/")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  )

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; (setq consult-async-min-input 2)

  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package consult-flycheck)

(use-package consult-ls-git
  :straight t
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :bind
  (("C-c n /" . consult-notes-search-in-all-notes)
   ("C-c n <" . consult-notes-org-roam-find-node)
   ("C-c n >" . consult-notes-org-roam-find-node-relation)
   )
  :config
  (setq consult-notes-sources
        '(("Posts"             ?p "~/.posts")
          ("Org"      ?r "~/.gclrc/org")))
  ;; Set org-roam integration OR denote integration
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  )

(use-package consult-projectile)

(use-package consult-yasnippet)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package visual-regexp)
(use-package visual-regexp-steroids)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; alien, hybrid
  (setq projectile-indexing-method 'alien projectile-enable-caching t)
  )

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c TAB"))  ; pick your own prefix key here
  :init
  (persp-mode)
  :config
  (setq persp-state-default-file (expand-file-name ".cache/gcl" user-emacs-directory))
  (setq persp-show-modestring 'header)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (use-package persp-projectile)
  )

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

;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (unless (server-running-p)
      (server-start))
  (error (message "Could not start server")))
