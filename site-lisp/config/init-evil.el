

(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(setq which-key-side-window-location 'bottom)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 0.1)
;;(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)

(require 'evil)
(evil-mode)

;; 退出编辑模式后光标留在原地
(setq evil-move-cursor-back nil)
;; 让回车，TAB，空格键保持原来的功能
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil))

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


(require 'evil-nerd-commenter)

(require 'evil-surround)


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
(global-evil-surround-mode 1)

(provide 'init-evil)
