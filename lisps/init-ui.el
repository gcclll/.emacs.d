;;; init-ui.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-defs))

;;----------------------------------------------------------------------------
;; `ui'
;;----------------------------------------------------------------------------
;; -END


;;----------------------------------------------------------------------------
;; `pretty-mode'
;;----------------------------------------------------------------------------
(use-package pretty-mode
  :init
  ;; (global-pretty-mode t)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org CheckBox Symbol"
                              ;; (push '("[ ]" .  "") prettify-symbols-alist)
                              ;; (push '("[X]" . "☑" ) prettify-symbols-alist)
                              ;; (push '("[-]" . "" ) prettify-symbols-alist)
                              (prettify-symbols-mode))))
;; -END


;;----------------------------------------------------------------------------
;; `themes'
;;----------------------------------------------------------------------------
;; DoomThemes
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-Iosvkem t))
;; -DoomThemes

;; DoomModeline
(use-package doom-modeline
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
 ;; -YASnippetPac
 :config
  (doom-modeline-mode))
;; -DoomModeline

;;----------------------------------------------------------------------------
;; `fonts'
;;----------------------------------------------------------------------------
;; FontsList
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("Input" . 12) ("SF Mono" . 13) ("Consolas" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(when *sys/gui*
  (change-font))
;; -END

;;----------------------------------------------------------------------------
;; `all-the-icons'
;;----------------------------------------------------------------------------
;; (use-package all-the-icons :if *sys/gui*)
;; -END

;;----------------------------------------------------------------------------
;; `emojify'
;;----------------------------------------------------------------------------
(use-package emojify
  :init (global-emojify-mode 1))
;; -END

;;----------------------------------------------------------------------------
;; `header2'
;;----------------------------------------------------------------------------
;; Header2Pac
(use-package header2
  :load-path (lambda () (expand-file-name "site-elisp/header2" user-emacs-directory))
  :custom
  (header-copyright-notice (concat "Copyright (C) 2020 " (user-full-name) "\n"))
  :hook (emacs-lisp-mode . auto-make-header)
  :config
  (add-to-list 'write-file-functions 'auto-update-file-header)
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2"))
;; -Header2Pac

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
