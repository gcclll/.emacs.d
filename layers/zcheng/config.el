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

(setq
 org-agenda-dir user-note-dir
 deft-dir user-note-dir
 blog-admin-dir user-note-dir)

(setq save-abbrevs nil)
(setq-default abbrev-mode t)
