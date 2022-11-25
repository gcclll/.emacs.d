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


;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings
	      '("C-`" "S-<delete>" "C-f" "C-b" "C-n" "C-p" "C-g" "<return>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
                                        ;； 临时强制转成英文模式，通过 rime-inline-ascii 恢复中文模式（什么鬼～～～）
                                        ;；　(define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
  ;; (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)

  (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.50/include")
  (setq-default rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  )

(provide 'init-rime)
