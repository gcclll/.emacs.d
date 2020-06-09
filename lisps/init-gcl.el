;;; init-gcl.el --- my gcl function
;;; Commentary:
;;; Code:

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
;; `gcl/rename-this-file-and-buffer'
;;----------------------------------------------------------------------------
(defun gcl/rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file !" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))
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
(defun gcl/bakup2 ()
  "Execute the command."
  (interactive)
  (gcl/exec-command "pythone3 ~/.gclrc/run.py"))

(defun gcl/bakup ()
  "Execute the command."
  (interactive)
  (let* ((command (or (and (boundp 'executable-command) executable-command) "python3 ~/.gclrc/run.py"))
           (compilation-ask-about-save nil))
      (executable-interpret (read-shell-command "Run: " command))))
;; -END

;;----------------------------------------------------------------------------
;; `run-shell'
;;----------------------------------------------------------------------------
(defun gcl/git-push ()
  "Execute the command."
  (interactive)
  (let* ((command (or (and (boundp 'executable-command)
                           executable-command)
                      (concat "~/.gclrc/shl/git-push.sh " (file-name-directory buffer-file-name))))
           (compilation-ask-about-save nil))
      (executable-interpret (read-shell-command "Run: " command))))
;; -END



(provide 'init-gcl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gcl.el ends here
