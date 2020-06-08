;;; init-mu4e.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; `mu4e'
;;----------------------------------------------------------------------------
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))
(use-package mu4e
  :ensure nil
  :ensure-system-package mu
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit nil)
  (mu4e-completing-read-function 'ivy-completing-read)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-drafts-folder "/Drafts")
  (mu4e-get-mail-command (format "mbsync -c '%s/isync/mbsyncrc' -a" xdg-config))
  (mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (mu4e-maildir "~/Maildir/gmail")
  (mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/All Mail" . ?a)
     ("/Drafts" . ?D)
     ("/Sent" . ?s)
     ("/Starred" . ?S)
     ("/Trash" . ?T)))
  (mu4e-org-contacts-file "~/github/documents/personal/contacts.org")
  (mu4e-refile-folder "/Archive")
  (mu4e-sent-folder "/Sent")
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-trash-folder "/Trash")
  (mu4e-update-interval 60)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  :config
  (add-to-list 'mu4e-headers-actions '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions '("org-contact-add" . mu4e-action-add-org-contact) t))
;; -END

;;----------------------------------------------------------------------------
;; `org-mu4e'
;;----------------------------------------------------------------------------
(use-package org-mu4e
  :ensure nil
  :custom
  (org-mu4e-convert-to-html t))
;; -END

;;----------------------------------------------------------------------------
;; `mu4e-alert'
;;----------------------------------------------------------------------------
(use-package mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))
;; -END

;;----------------------------------------------------------------------------
;; `message'
;;----------------------------------------------------------------------------
(use-package message
  :ensure nil
  :custom (send-mail-function 'smtpmail-send-it))
;; -END

;;----------------------------------------------------------------------------
;; `smtpmail'
;;----------------------------------------------------------------------------
(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-smtp-user "gccll.love")
  (smtpmail-stream-type 'starttls))
;; -END

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
