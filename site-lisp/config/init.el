;;; 参考配置
(setq debug-on-error t)
(setq debug-on-quit t)

;;; https://github.com/purcell/emacs.d
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(and (file-readable-p custom-file) (load custom-file))

(setq
 ;; 不要缩放frame.
 frame-inhibit-implied-resize t
 ;; 默认用最简单的模式
 initial-major-mode 'fundamental-mode
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "MANPATH")
      exec-path-from-shell-arguments '("-l"))
(exec-path-from-shell-initialize)

;; 定义一些启动目录，方便下次迁移修改
(defvar gcl-emacs-root-dir (file-truename "~/.emacs.d/site-lisp"))
(defvar gcl-emacs-config-dir (concat gcl-emacs-root-dir "/config"))
(defvar gcl-emacs-extension-dir (concat gcl-emacs-root-dir "/extensions"))

(with-temp-message "" ;抹掉插件启动的输出
  ;; --- benchmark
  ;; (require 'benchmark-init)
  ;; (require 'benchmark-init-modes)
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)


    (add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)


;;   (require 'init-fullscreen)

  ;; --- basic
  (require 'init-funcs)
;;   (require 'init-hacking)
;;   (require 'init-basic)

;;   (require 'init-performance)
;;   (require 'init-ui)
;;   (require 'init-buffer)
;;   (require 'init-evil)

  (setq-default bidi-display-reordering nil)

  ;; 可以延后加载的
  (run-with-idle-timer
   1 nil
   #'(lambda ()
        ;; TODO
       ))
  )


;; 直接将环境变量拷贝到 ~/.path 中
;; sh -c 'printf "%s" "$PATH"' > ~/.path
(condition-case err
    (let ((path (with-temp-buffer
		              (insert-file-contents-literally "~/.path")
		              (buffer-string))))
      (setenv "PATH" path)
      (setq exec-path (append (parse-colon-path path) (list exec-directory))))
  (error (warn "%s" (error-message-string err))))


;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (unless (server-running-p)
      (server-start))
  (error (message "Could not start server")))


(message "-> done, enjoy it !!!")

(provide 'init)
