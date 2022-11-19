
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

(require 'so-long)
(add-hook 'after-init-hook #'global-so-long-mode)
(setq so-long-threshold 40000)

(provide 'init-performance)