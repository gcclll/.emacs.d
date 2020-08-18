;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

(require 'cl-lib)

(require 'init-const)
(require 'init-custom)

;; Suppress warnings
(defvar circadian-themes)
(defvar socks-noproxy)
(defvar socks-server)

(declare-function async-inject-variables 'async)
(declare-function chart-bar-quickie 'chart)
(declare-function flycheck-buffer 'flycheck)
(declare-function flymake-start 'flymake)
(declare-function upgrade-packages 'init-package)

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))
(global-set-key (kbd "s-r") #'revert-this-buffer)

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(global-set-key (kbd "C-x K") #'delete-this-file)

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (message "WARNING: Current buffer is not attached to a file!")))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'centaur-reload-init-file #'reload-init-file)
(global-set-key (kbd "C-c C-l") #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url centaur-homepage))

;; Open custom file
(defun open-custom-file()
  "Open or create `custom-file'."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p centaur-custom-example-file)
        (copy-file centaur-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" centaur-custom-example-file)))
  (find-file custom-file)
  (find-file-other-window centaur-custom-post-file))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and centaur-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun centaur-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.
Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(define-minor-mode centaur-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group centaur
  (if centaur-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +2))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))
(global-set-key (kbd "M-<f7>") #'centaur-read-mode)

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car centaur-package-archives-alist)))))
  ;; Set option
  (centaur-set-variable 'centaur-package-archives archives no-save)

  ;; Refresh if need
  ;; (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'centaur-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun centaur-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.
Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((urls (mapcar
                (lambda (url)
                  (concat url "archive-contents"))
                (mapcar #'cdr
                        (mapcar #'cadr
                                (mapcar #'cdr
                                        centaur-package-archives-alist)))))
         (durations (mapcar
                     (lambda (url)
                       (let ((start (current-time)))
                         (message "Fetching %s..." url)
                         (cond ((executable-find "curl")
                                (call-process "curl" nil nil nil "--max-time" "10" url))
                               ((executable-find "wget")
                                (call-process "wget" nil nil nil "--timeout=10" url))
                               (t (user-error "curl or wget is not found")))
                         (float-time (time-subtract (current-time) start))))
                     urls))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            centaur-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "%s" urls)
    (message "%s" durations)
    (message "%s is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen."
  (and sys/mac-cocoa-p
       emacs/>=26p
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))



;; Update
(defun update-config ()
  "Update Centaur Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating configurations...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'centaur-update-config #'update-config)

(defvar centaur--updating-packages nil)
(defun update-packages (&optional sync)
  "Refresh package contents and update all packages.
If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (when centaur--updating-packages
    (user-error "Still updating packages..."))

  (message "Updating packages...")
  (if (and (not sync)
           (require 'async nil t))
      (progn
        (setq centaur--updating-packages t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "\\`\\(load-path\\)\\'")
            (require 'init-funcs)
            (require 'init-package)
            (upgrade-packages)
            (with-current-buffer auto-package-update-buffer-name
              (buffer-string)))
         (lambda (result)
           (setq centaur--updating-packages nil)
           (message "%s" result)
           (message "Updating packages...done"))))
    (progn
      (upgrade-packages)
      (message "Updating packages...done"))))
(defalias 'centaur-update-packages #'update-packages)

(defvar centaur--updating nil)
(defun update-config-and-packages(&optional sync)
  "Update confgiurations and packages.
If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (when centaur--updating
    (user-error "Centaur Emacs is still updating..."))

  (message "This will update Centaur Emacs to the latest")
  (if (and (not sync)
           (require 'async nil t))
      (progn
        (setq centaur--updating t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "\\`\\(load-path\\)\\'")
            (require 'init-funcs)
            (require 'init-package)
            (update-config)
            (update-packages t)
            (with-current-buffer auto-package-update-buffer-name
              (buffer-string)))
         (lambda (result)
           (setq centaur--updating nil)
           (message "%s" result)
           (message "Done. Restart to complete process"))))
    (progn
      (update-config)
      (update-packages t)
      (message "Done. Restart to complete process"))))
(defalias 'centaur-update #'update-config-and-packages)

(defun update-all()
  "Update dotfiles, org files, Emacs confgiurations and packages to the latest versions."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'centaur-update-all #'update-all)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'centaur-update-dotfiles #'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'centaur-update-org #'update-org)



;; UI
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun centaur--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme centaur-theme-alist) theme))

(defun centaur-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random))
      (string-prefix-p "doom" (symbol-name (centaur--theme-name theme)))))

(defun centaur-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun centaur-theme-enable-p (theme)
  "The THEME is enabled or not."
  (and (not (memq centaur-theme '(auto random)))
       (memq (centaur--theme-name theme) custom-enabled-themes)))

(defun centaur--load-theme (theme)
  "Disable others and enable new one."
  (message "Loading theme `%s'" theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun centaur-load-random-theme ()
  "Load the random theme."
  (interactive)
  (let* ((themes (mapcar #'cdr centaur-theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if theme
        (centaur--load-theme theme)
      (user-error "Failed to load `random' theme"))))

(defun centaur-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list (intern (completing-read
                  "Load theme: "
                  `(auto random ,@(mapcar #'car centaur-theme-alist))))))
  ;; Set option
  (centaur-set-variable 'centaur-theme theme no-save)

  (pcase centaur-theme
    ('auto
     ;; Time-switching themes
     (use-package circadian
       :functions circadian-setup
       :custom (circadian-themes centaur-auto-themes)
       :init (circadian-setup)))
    ('random (centaur-load-random-theme))
    (_ (centaur--load-theme (centaur--theme-name theme)))))
(global-set-key (kbd "C-c T") #'centaur-load-theme)



;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" centaur-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,centaur-proxy)
          ("https" . ,centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1086 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

;;----------------------------------------------------------------------------
;; `gcl/lcd'
;;----------------------------------------------------------------------------
(defun gcl/lcd ()
  "Fzf lcd."
  (interactive)
  (fzf/start default-directory
             (fzf/grep-cmd "lcd" "-l %s")))
;; -END

;;----------------------------------------------------------------------------
;; `gcl/open-in-finder'
;;----------------------------------------------------------------------------
(defun gcl/open-in-finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file!"))))
;; -END

;;----------------------------------------------------------------------------
;; `gcl/search'
;;----------------------------------------------------------------------------
(defun gcl/search (query-url prompt)
  "Open the search url constructed with tht QUERY-URL.
PROMPT sets the `read-string' prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))
;; -END

;;----------------------------------------------------------------------------
;; `gcl/install-search-engine'
;;----------------------------------------------------------------------------
(defmacro gcl/install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them, args: SEARCH-ENGINE-NAME, SEARCH-ENGINE-URL, SEARCH-ENGINE-PROMPT."
  `(defun ,(intern (format "gcl-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (gcl/search ,search-engine-url ,search-engine-prompt)))
;; -END


;;----------------------------------------------------------------------------
;; `gcl/delete-this-file-make-backup'
;;----------------------------------------------------------------------------
(defun gcl/delete-this-file-make-backup (&optional @no-backup-p)
  "Delete current file, makes a backup~, closes the buffer, args @NO-BACKUP-P."
  (interactive "P")
  (let* (
         ($fname (buffer-file-name))
         ($buffer-is-file-p $fname)
         ($backup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if $buffer-is-file-p
        (progn
          (save-buffer $fname)
          (when (not @no-backup-p)
            (copy-file
             $fname
             (concat "/tmp/" (file-name-nondirectory $fname) $backup-suffix)
             t))
          (delete-file $fname)
          (message "Deleted. Backup created at 「%s」." (concat "/tmp/" (file-name-nondirectory $fname) $backup-suffix)))
      (when (not @no-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" $backup-suffix))
        (message "Backup created at 「%s」." (concat "xx" $backup-suffix))))
    (kill-buffer (current-buffer))))
;; -END

;;----------------------------------------------------------------------------
;; `gcl/delete-this-file'
;;----------------------------------------------------------------------------
(defun gcl/delete-this-file (&optional @no-backup-p)
  "Delete current file or directory of dired, arg."
  (interactive "P")
  (if (eq major-mode 'dired-mode)
      (progn
        (message "you in dired. nothing's done."))
    (let (($bstr (buffer-string)))
      (when (> (length $bstr) 0)
        (if (< (point-max) 1000000)
            (kill-new $bstr)
          (message "Content not copied. buffer size is greater than 1 megabytes.")))
      (if (buffer-file-name)
          (gcl/delete-this-file-make-backup @no-backup-p)
        (when (buffer-file-name)
          (when (file-exists-p (buffer-file-name))
            (progn
              (delete-file (buffer-file-name))
              (message "Deleted file: 「%s」." (buffer-file-name)))))
        (let ((buffer-offer-save nil))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))))))
;; -END

;;----------------------------------------------------------------------------
;; `gcl/open-in-external-app'
;;----------------------------------------------------------------------------
(defun gcl/open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
When called in Emacs Lisp, if @fname is given, open that.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))
;; -END
;;----------------------------------------------------------------------------
;; `gcl/run-command'
;;----------------------------------------------------------------------------
(defun gcl/exec-command (cmd)
  "Execute the command: `CMD` with description ARGS."
  (interactive)
  (let* ((command (or (and (boundp 'executable-command) executable-command) cmd))
         (compilation-ask-about-save nil))
    (executable-interpret (read-shell-command "Run: " command))))
;; -END

;;----------------------------------------------------------------------------
;; `run-python'
;;----------------------------------------------------------------------------
(defun gcl/bakup ()
  "Execute the command."
  (interactive)
  (gcl/exec-command "pythone3 ~/.gclrc/run.py"))
;; -END

;;----------------------------------------------------------------------------
;; `run-shell'
;;----------------------------------------------------------------------------

(defun git-push-delete-window ()
	(progn
		(message "Git push should done or running in background.")
		(delete-window)))

(defun gcl/git-push ()
  "Execute the command."
  (interactive)
	(progn
		(gcl/exec-command (concat "~/.gclrc/shl/git-push.sh " (file-name-directory buffer-file-name)))
		;; second, repeat, func, message
		(run-at-time "1 sec" nil 'git-push-delete-window)))
;; -END

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
