
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

;; --- bookmark
;; 有变化时自动保存
(setq bookmark-save-flag 1)

;; --- node evn varirable
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

(provide 'init-basic)
