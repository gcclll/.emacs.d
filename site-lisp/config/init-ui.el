(set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono")

(use-package font-lock+
  :straight (:host github :repo "emacsmirror/font-lock-plus"))

(use-package all-the-icons
  :if (display-graphic-p))
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
  ;; :config
  ;; (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  )

;; -- 丰富括号
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(provide 'init-ui)
