;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
;; (setq centaur-theme 'light)                    ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode)) ; Ignore format on save for some languages
;; (setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; (setq centaur-benchmark-init t)                ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-package-archives 'emacs-china)
 '(centaur-theme 'default)
 '(custom-safe-themes
	 '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(package-selected-packages
	 '(lsp-vue ztree zoom youdao-dictionary yasnippet-snippets xterm-color which-key web-mode vterm volatile-highlights vimrc-mode use-package treemacs-projectile treemacs-persp treemacs-magit toc-org tldr tide symbol-overlay swift-mode sudo-edit solaire-mode smartparens smart-region skewer-mode shell-pop shackle scss-mode scala-mode rmsbolt rjsx-mode rg restclient restart-emacs react-snippets ranger rainbow-mode rainbow-delimiters quickrun protobuf-mode pretty-hydra prettier-js powershell pomidor plantuml-mode php-mode persp-mode-projectile-bridge persistent-scratch pdf-view-restore paradox pager ox-gfm overseer osx-dictionary origami org-tree-slide org-roam-server org-rich-yank org-preview-html org-pomodoro org-mime org-fancy-priorities org-bullets olivetti ob-rust ob-mermaid ob-ipython ob-go nov mwim move-text modern-cpp-font-lock mocha mixed-pitch minions mermaid-mode memory-usage markdown-toc magit-todos macrostep lua-mode lsp-ui lsp-sourcekit lsp-pyright lsp-julia lsp-java lsp-ivy live-py-mode list-environment linum-off js2-refactor js-doc ivy-yasnippet ivy-xref ivy-prescient ivy-hydra iedit ibuffer-projectile hungry-delete howdoyou hlinum highlight-parentheses highlight-indent-guides highlight-defined hide-mode-line helpful helm-dash haml-mode grip-mode goto-line-preview goto-last-point goto-char-preview go-tag go-playground go-impl go-gen-test go-fill-struct go-dlv gnu-elpa-keyring-update gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger general forge focus flyspell-correct-ivy flycheck-posframe flycheck-golangci-lint fish-mode find-file-in-project fd-dired fancy-narrow eyebrowse exec-path-from-shell evil-surround evil-mc-extras evil-matchit evil-commentary esup eshell-z eshell-prompt-extras esh-help esh-autosuggest emojify emmet-mode elfeed editorconfig easy-kill-extras dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode docker disk-usage diredfl dired-rsync dired-quick-sort dired-git-info diminish diffview diff-hl default-text-scale dashboard daemons csv-mode css-eldoc csharp-mode crux counsel-world-clock counsel-tramp counsel-projectile counsel-osx-app copyit company-quickhelp company-prescient company-lsp company-emoji company-box comment-dwim-2 coffee-mode ccls cask-mode calfw-org calfw-ical calfw cal-china-x browse-at-remote bongo bmx-mode benchmark-init beginend avy-zap auto-package-update atomic-chrome anzu amx all-the-icons-ivy-rich all-the-icons-ibuffer all-the-icons-dired aggressive-indent add-node-modules-path ace-pinyin ace-link))
 '(safe-local-variable-values '((org-attach-directory . "../media/img/"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:background "#23272e" :extend t))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold))))))

;;; custom.el ends here
