
;; --- marginalia
(require 'marginalia)
(setq marginalia-max-relative-age 0)
(setq marginalia-align 'right)
(marginalia-mode)

(require 'all-the-icons-completion)
(add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)
(all-the-icons-completion-mode)

;; --- vertico
(require 'vertico)
(require 'vertico-repeat)
(add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
(setq vertico-count 13
      vertico-resize t
      vertico-cycle nil)
(vertico-mode)

;; --- embark
(require 'embark)
(general-define-key
 "C-." 'embark-act
 "C-;" 'embark-dwim
 "C-h B" 'embark-bindings)

(setq prefix-help-command 'embark-prefix-help-command)
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

;; --- orderless
(require 'orderless)
(setq completion-styles '(orderless-fast)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

(provide 'init-buffer)
