;;; init-gcl.el --- my gcl function
;;; Commentary:
;;; Code:

(defun gcl/lcd ()
  "Fzf lcd."
  (interactive)
  (fzf/start default-directory
             (fzf/grep-cmd "lcd" "-l %s")))

(defun gcl/open-in-finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file!"))))

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

(defun gcl/search (query-url prompt)
  "Open the search url constructed with tht QUERY-URL.
PROMPT sets the `read-string' prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro gcl/install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them, args: SEARCH-ENGINE-NAME, SEARCH-ENGINE-URL, SEARCH-ENGINE-PROMPT."
  `(defun ,(intern (format "gcl-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (gcl/search ,search-engine-url ,search-engine-prompt)))


(gcl/install-search-engine "google" "http://www.google.com/search?q=" "Google: ")
(gcl/install-search-engine "youtube" "http://www.youtube.com/results?search_query=" "Search YouTuBe: ")
(gcl/install-search-engine "github" "https://github.com/search?q=" "Search GitHub: ")
(gcl/install-search-engine "duckduckgo" "https://duckduckgo.com?t=lm&q=" "Search DuckDuckGo: ")
(gcl/install-search-engine "baidu" "https://www.baidu.com/s?wd=" "Search BaiDu: ")

(provide 'init-gcl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gcl.el ends here
