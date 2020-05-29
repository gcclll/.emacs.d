;;; init.el --- -*- lexical-binding: t -*-
;; clone from https://github.com/MatthewZMD/.emacs.d
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: ZhiCheng Lee
;; Copyright (C) 2020 ZhiCheng Lee
;; Created: Fri May 29 2020 10:52:11 GMT+0800 (China Standard Time)
;; Version: 1.0.0
;; Last-Updated: Fri May 29 22:27:27 2020 (+0800)
;;           By: ZhiCheng Lee
;; URL: https://github.com/gcclll/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "lisps" user-emacs-directory))
;; -LoadPath

(require 'init-pre)
(require 'init-defs)
(require 'init-package)
(require 'init-global)
(require 'init-func)
(require 'init-gcl)
(require 'init-evil)
(require 'init-search)
(require 'init-crux)
(require 'init-avy)
(require 'init-misc)
(require 'init-shell)
(require 'init-dired)
(require 'init-buffer)
(require 'init-ui)
(require 'init-dashboard)
(require 'init-projectile)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-dev)
(require 'init-parens)
(require 'init-indent)
(require 'init-edit)
(require 'init-lsp)
(require 'init-company)
(require 'init-eyeb)

(require 'init-cc)
(require 'init-python)
(require 'init-latex)
(require 'init-webdev)
;; Miscellaneous
(require 'init-org)
(require 'init-eaf)
;; (require 'init-erc)
;; (require 'init-eww)
;; (require 'init-mu4e)
(require 'init-tramp)
(require 'init-pdf)
(require 'init-leetcode)
(require 'init-pyim)
;; (require 'init-epaint)
;; (require 'init-games)
(require 'init-general)
(require 'init-log)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (diminish auto-package-update use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
