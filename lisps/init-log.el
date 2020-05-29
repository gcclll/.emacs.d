;;; init-log.el --- log motions
;;; Commentary:
;;; Code:

(use-package wakatime-mode
  :diminish 'wakatime-mode
  :hook (after-init . global-wakatime-mode)
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (setq wakatime-python-bin nil)
  (global-wakatime-mode)
  (setq wakatime-api-key "497a474b-5512-4e56-82a7-0b56c64cd75c"))


(provide 'init-log)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-log.el ends here
