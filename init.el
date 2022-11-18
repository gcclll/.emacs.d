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

;; Add diminish, which makes it easier to customize lighters (minor mode display)
(use-package diminish)

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

(set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono")

(use-package font-lock+
  :straight (:host github :repo "emacsmirror/font-lock-plus"))

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
    (evil-global-set-key 'normal "w" 'evil-avy-goto-word-1)
    (evil-global-set-key 'motion "-" 'org-decrease-number-at-point)
    (evil-global-set-key 'motion "+" 'org-increase-number-at-point)
    )
  )