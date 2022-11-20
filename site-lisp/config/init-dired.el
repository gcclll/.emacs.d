(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "open")
        ("\\.docx\\'" "open")
        ("\\.\\(?:djvu\\|eps\\)\\'" "open")
        ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
        ("\\.\\(?:xcf\\)\\'" "open")
        ("\\.csv\\'" "open")
        ("\\.tex\\'" "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
         "open")
        ("\\.\\(?:mp3\\|flac\\)\\'" "open")
        ("\\.html?\\'" "open")
        ("\\.md\\'" "open")))

;; --- diredfl
(require 'diredfl)
(add-hook 'dired-mode-hook 'diredfl-mode)

(provide 'init-dired)
