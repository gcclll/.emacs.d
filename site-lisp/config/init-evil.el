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
    ;; (define-key evil-motion-state-map (kbd "RET") nil)
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

    (evil-global-set-key 'normal (kbd "gm") 'magit)
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

(evil-set-initial-state 'multi-vterm-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'magit-branch-manager-mode 'emacs)

(provide 'init-evil)
