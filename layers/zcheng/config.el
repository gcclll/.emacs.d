                                        ; -*- lexical-binding: t -*-
;; (golden-ratio-mode 1)

(setq sunshine-appid "6621f1828db7491591a32d1391539ac5")
(setq css-indent-offset 2)
(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(if (spacemacs/system-is-mswindows)
    (setq
     org-agenda-dir "d:/.blog/zcheng.top/orgs"
     deft-dir "d:/.blog/zcheng.top/orgs"
     blog-admin-dir "d:/.blog/zcheng.top")
  (setq
   org-agenda-dir "~/.blog/zcheng.top/orgs"
   deft-dir "~/.blog/zcheng.top/orgs"
   blog-admin-dir "~/.blog/zcheng.top"))
