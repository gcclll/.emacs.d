;;; config.el --- summary -*- lexical-binding: t -*-

;; Author: Zhicheng Lee
;; Maintainer: Zhicheng Lee
;; Email: gccll.love@gmail.com
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://www.zcheng.top
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


(when (spacemacs/window-system-is-mac)
  (setq ns-pop-up-frames nil))
(global-prettify-symbols-mode 1)
(setq-default fill-column 80)
;; prevent dired window press o to split into three column
(setq-default split-width-threshold 200)
(delete-selection-mode t)
;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/

(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;;Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;; cleanup recent files
(defun zilongshanren/cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))

(add-hook 'kill-emacs-hook #'zilongshanren/cleanup-recentf)
(setq tags-add-tables nil)
(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)
(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)



(provide 'config)

;;; config.el ends here
