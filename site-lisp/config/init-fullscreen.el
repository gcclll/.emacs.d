
;; --- 全屏
(if (featurep 'cocoa)
    (progn
      (setq ns-use-native-fullscreen nil)
      (setq ns-use-fullscreen-animation nil)

      ;; 默认先最大化。
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

      (run-at-time "2sec" nil
                   (lambda ()
                     (toggle-frame-fullscreen)
                     )))
  ;; 非Mac平台直接全屏
  (require 'fullscreen)
  (fullscreen))

(provide 'init-fullscreen)