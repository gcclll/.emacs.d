;;; init-erc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-global)
  (require 'init-func))

;;----------------------------------------------------------------------------
;; `erc', https://github.com/rememberYou/.emacs.d/blob/master/config.org
;;----------------------------------------------------------------------------
(use-package erc
  :delight "ε "
  :preface
  (defun my/erc-browse-last-url ()
    "Searchs backwards through an ERC buffer, looking for a URL. When a URL is
     found, it prompts you to open it."
    (interactive)
    (save-excursion
      (let ((ffap-url-regexp "\\(https?://\\)."))
        (ffap-next-url t t))))

  (defun my/erc-count-users ()
    "Displays the number of users and ops connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let ((hash-table (with-current-buffer (erc-server-buffer)
                                  erc-server-users))
                    (users 0)
                    (ops 0))
                (maphash (lambda (k v)
                           (when (member (current-buffer)
                                         (erc-server-user-buffers v))
                             (incf users))
                           (when (erc-channel-user-op-p k)
                             (incf ops)))
                         hash-table)
                (message "%d users (%s ops) are online on %s" users ops channel))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my/erc-get-ops ()
    "Displays the names of ops users on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (if (and (cdr cdata)
                                    (erc-channel-user-op (cdr cdata)))
                               (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str (string-trim (replace-regexp-in-string "\n+" " " str))))

  (defun my/erc-reset-track-mode ()
    "Resets ERC track mode."
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-modified-channels-display)
    (force-mode-line-update))

  (defun my/erc-start-or-switch ()
    "Connects to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (erc-track-switch-buffer 1)
      (erc :server "irc.freenode.net" :port 6667 :nick "zhicheng")))
  :hook ((ercn-notify . my/erc-notify)
         (erc-send-pre . my/erc-preprocess))
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "#bash" "#vuejs"
                                  "#emacs" "#i3" "#latex" "#org-mode" "#python"
                                  "#typescript" "#reactjs" "#javascript" "#nodejs")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format "%n on %t (%m)")
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'bury)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package erc-hl-nicks :after erc)
(use-package erc-image :after erc)
;; -END

(provide 'init-erc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-erc.el ends here
