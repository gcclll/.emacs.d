;;; init-func.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-global))

;;----------------------------------------------------------------------------
;; `resize-window-width'
;;----------------------------------------------------------------------------
(defun resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))
;; -END

;;----------------------------------------------------------------------------
;; `resize-window-height'
;;----------------------------------------------------------------------------
(defun resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))
;; -END


;;----------------------------------------------------------------------------
;; `resize-window'
;;----------------------------------------------------------------------------
(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))
;; -END


;;----------------------------------------------------------------------------
;; `edit-configs'
;;----------------------------------------------------------------------------
(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/init.org"))
;; -END

;;----------------------------------------------------------------------------
;; `save-and-update-includes'
;;----------------------------------------------------------------------------
(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in `org-mode', so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))
;; -END
;; OrgIncludeAuto

;;----------------------------------------------------------------------------
;; `decide-line-range'
;;----------------------------------------------------------------------------
(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1))))))
;; -END

;;----------------------------------------------------------------------------
;; `abort-minibuffer-using-mouse'
;;----------------------------------------------------------------------------
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
;; -END

;;----------------------------------------------------------------------------
;; `display-line-overlay+'
;;----------------------------------------------------------------------------
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background null :inherit highlight)))
    ol))
;; -END

;;----------------------------------------------------------------------------
;; `read-lines'
;;----------------------------------------------------------------------------
(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer (insert-file-contents file-path)
                    (split-string (buffer-string) "\n" t)))
;; -END

;;----------------------------------------------------------------------------
;; `where-am-i'
;;----------------------------------------------------------------------------
(defun where-am-i ()
  "An interactive function showing function `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))
;; -END

;;----------------------------------------------------------------------------
;; `my-pclip'
;;----------------------------------------------------------------------------
(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe")))
    (cond
     ;; Windows
     ((fboundp 'w32-set-clipboard-data)
      (w32-set-clipboard-data str-val))

     ;; Windows 10
     ((and win64-clip-program)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))

     ;; xclip can handle
     (t
      (xclip-set-selection 'clipboard str-val)))))
;; -END

;;----------------------------------------------------------------------------
;; `delete-trailing-whitespace-except-current-line'
;;----------------------------------------------------------------------------
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))
;; -END

(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
