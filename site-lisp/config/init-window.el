;; --- winum
(use-package winum
  :init
  (winum-mode))

;; --- window-numbering
(use-package window-numbering
  :hook (after-init . window-numbering-mode))

;; --- toggle-one-window
(require 'toggle-one-window)

;; --- watch-other-window
(require 'watch-other-window)

;; --- misc
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


(provide 'init-window)
