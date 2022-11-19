(set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono")

(require 'font-lock+)
(require 'all-the-icons)
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; -- header line
(set-face-attribute 'header-line nil
                    :foreground (face-attribute 'mode-line :foreground)
                    :background (face-attribute 'mode-line :background)
                    ;; height of mode-line is also unspecified, so we set it directly.
                    :height 150
                    :box (face-attribute 'mode-line :box))

;; -- awesome tray
(require 'awesome-tray)
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

(awesome-tray-mode 1)


;; -- highlight-parentheses
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
;; (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

;; -- 丰富括号
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(provide 'init-ui)
