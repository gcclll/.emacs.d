.emacs.d

all copy from https://github.com/MatthewZMD/.emacs.d

## issues

1. 环境变量问题(在 `.profile` 中设置)

   https://emacs-china.org/t/exec-path-from-shell/2515/9

   新增 `~/.zshenv` 或`~/.profile` 然后将需要的环境变量设置到这两个文件。

   配置：

   ```lisp
   (use-package exec-path-from-shell
     :ensure t
     :if (memq window-system '(mac ns x))
     :config
     (setq exec-path-from-shell-variables '("PATH" "RGPATH"))
     ;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
     (setq exec-path-from-shell-check-startup-files nil) 
     (setq exec-path-from-shell-arguments '("-l"))
     (exec-path-from-shell-initialize))
   ```

   

# packages

## <font color="red">TODOs</font>

1. [restclient](https://github.com/pashky/restclient.el)

## others

### all-the-icons

## inits

### init-pre.el

configuration before anything.

### init-defs.el

Vars and funcs.

### init-package.el

Use-package.

### init-global.el

global configurations, tiny settings.

### init-func.el

func def after use-package.

### init-gcl.el

my functions.

1. delete current file and buffer, and backup to /tmp

   ```lisp
   
   (defun gcl/delete-this-file-make-backup (&optional @no-backup-p)
     "Delete current file, makes a backup~, closes the buffer."
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
   
   
   ```

   

### init-search.el

ivy, search(rg, color-rg, ...)

### init-crux.el

https://github.com/bbatsov/crux

| k       | d                           |
| ------- | --------------------------- |
| C-a     | crux-move-beginning-of-line |
| C-x 4 t | crux-transpose-windows      |
| C-x K   | crux-kill-other-buffers     |
| C-k     | crux-smart-kill-line        |

### init-ivy.el

| k     | d                   |
| ----- | ------------------- |
| C-z c | Avy-goto-char-timer |
| C-z l | Avy-goto-line       |

### init-misc.el

1. winner
2. [which-key](https://github.com/justbur/emacs-which-key)
3. [popup-kill-ring](https://github.com/waymondo/popup-kill-ring)
4. [undo-tree](https://github.com/tarsiiformes/undo-tree)
5. discover-my-major
6. zone-choose

### init-shell.el

### init-dired.el

| k       | d                       |
| ------- | ----------------------- |
| C-x C-j | dired-jump              |
| C-x j   | dired-jump-other-window |
| C-x C-s |                         |
| C-x C-s | save-all-buffers        |

### init-buffer.el

| k       | d       |
| ------- | ------- |
| C-x C-b | ibuffer |

all buffer list.

### init-ui.el

ui + themes(doom-themes)

1. [unicode symbols](https://unicodelookup.com)
2. [doom-themes](https://github.com/hlissner/emacs-doom-themes)
3. header2

### init-dashboard.el

| k     | d                          |
| ----- | -------------------------- |
| C-z d | open-dashboard             |
| n     | dashboard-next-line        |
| p     | dashboard-previous-line    |
| N     | dashboard-next-section     |
| F     | dashboard-previous-section |

![](http://qiniu.ii6g.com/1590741049.png?imageMogr2/thumbnail/!100p)

### init-project.el

| k     | b                            |
| ----- | ---------------------------- |
| C-x g | magit-status                 |
| C-c p | projectile-command-map       |
| C-z p | projectile-add-known-project |
| M-0   | treemacs-select-window       |

treemacs

| k         | b                             |
| --------- | ----------------------------- |
| M-0       | treemacs-select-window        |
| C-x t 1   | treemacs-delete-other-windows |
| C-x t t   | treemacs                      |
| C-x t B   | treemacs-bookmark             |
| C-x t C-t | treemacs-find-file            |
| C-x t M-t | treemacs-find-tag             |
| C-p       | treemacs-previous-line        |

### init-yasnippet.el

### init-flycheck.el

### init-dev.el

programmer development.

1. dumb-jump

2. quickrun

   | k    | b              |
   | ---- | -------------- |
   | f5   | quickrun       |
   | M-f5 | quickrun-shell |

3. [format-all](https://github.com/emacsmirror/format-all)
   q: [postframe not found](https://github.com/flycheck/flycheck/issues/963)

   >Hi [@fjs2097](https://github.com/fjs2097)
   >Try running `package-refresh-contents` before running `package-install`.
   >Or install `flycheck` from the packages list after running `package-list-packages`.

   | k       | b                 |
   | ------- | ----------------- |
   | C-c C-f | format-all-buffer |

4. evil-nerd-commenter

   | k       | b                                 |
   | ------- | --------------------------------- |
   | C-c M-; | c-toggle-comment-style            |
   | M-;     | evilnc-comment-or-uncomment-lines |

5. [ein(Emacs IPython Notebook)](https://melpa.org/#/ein)

### init-parens.el

| k                 | b                               |
| ----------------- | ------------------------------- |
| "C-M-f"           | sp-forward-sexp                 |
| "C-M-b"           | sp-backward-sexp                |
| "C-M-a"           | sp-backward-down-sexp           |
| "C-M-e"           | sp-up-sexp                      |
| "C-M-w"           | p-copy-sexp                     |
| "C-M-k"           | sp-change-enclosing             |
| "M-k"             | sp-kill-sexp                    |
| `C-M-<backspace>` | sp-splice-sexp-killing-backward |
| `C-S-<backspace>` | sp-splice-sexp-killing-around   |
| "C-]"             | sp-select-next-thing-exchange   |

### init-indent.el

### init-edit.el

| k     | b                 |
| ----- | ----------------- |
| C-z , | iedit-mode        |
| M-D   | awesome-pair-kill |

1. awesome-pair

   | k    | b                       |
   | ---- | ----------------------- |
   | M-D  | awesome-pair-kill       |
   | SPC  | awesome-pair-space      |
   | =    | awesome-pair-equal      |
   | M-F  | awesome-pair-jump-right |
   | M-B  | awesome-pair-jump-left  |

2. conf-mode

3. delete-block

   | k               | b                     |
   | --------------- | --------------------- |
   | M-d             | delete-block-forward  |
   | `C-<backspace>` | delete-block-backward |
   | `M-<backspace>` | delete-block-backward |
   | M-DEL           | delete-block-backward |

### init-lsp.el

| k       | b                      |
| ------- | ---------------------- |
| C-c C-f | lsp-format-buffer      |
| C-c u   | lsp-ui-imenu           |
| M-i     | lsp-ui-doc-focus-frame |

dap-mode

| k       | b                     |
| ------- | --------------------- |
| f12     | dap-debug             |
| f8      | dap-continue          |
| f9      | dap-next              |
| M-f11   | dap-step-in           |
| C-M-f11 | dap-step-out          |
| f7      | dap-breakpoint-toggle |

### init-company.el

| k     | b                     |
| ----- | --------------------- |
| M-q   | company-other-backend |
| C-z t | company-tabnine       |

### init-cc.el

### init-python.el

### init-latex.el

### init-webdev.el

| k     | b                  |
| ----- | ------------------ |
| C-z < | instant-rename-tag |

### init-org.el

| k       | b                          |
| ------- | -------------------------- |
| C-c l   | org-store-link             |
| C-c a   | org-agenda                 |
| C-c c   | org-capture                |
| C-c b   | org-switch                 |
| C-c C-p | org-export-as-pdf-and-open |

### init-eaf.el

[Emacs Application Framework (EAF)](https://github.com/manateelazycat/emacs-application-framework.git)

可以直接打开其他软件。

| k     | b                     |
| ----- | --------------------- |
| M-q   | nil                   |
| C-M-s | open_link             |
| RET   | scroll_up             |
| DEL   | scroll_down_page      |
| u     | scroll_down_page      |
| d     | scroll_up_page        |
| M->   | scroll_to_end         |
| M-<   | scroll_to_home        |
| q     | quit-window           |
| C-=   | zoom_in               |
| C--   | zoom_out              |
| p     | take_photo            |
| M-]   | eaf-send-key-sequence |

### init-erc.el

[ERC is a powerful, modular, and extensible IRC client for Emacs](https://github.com/pymander/erc)



| k       | b                    |
| ------- | -------------------- |
| M-z i   | erc-start-or-switch  |
| C-c C-b | erc-switch-to-buffer |
| M-RET   | new-line             |

### init-eww.el

### init-mu4e.el

| k     | b    |
| ----- | ---- |
| M-z m | mu4e |

### init-tramp.el

ftp tool.

### init-pdf.el

### init-leetcode.el

https://github.com/kaiwk/leetcode.el/tree/28b78c45c86570cb1e3538f275eb4de1cf28cd04

| keymap | command                                |
| ------ | -------------------------------------- |
| n      | cursor move down                       |
| p      | cursor move up                         |
| s      | filter problem by regex                |
| t      | filter problem by tag                  |
| /      | clear filters                          |
| g      | refresh without fetching from LeetCode |
| G      | refresh all data                       |
| RET    | show current problem description       |

### init-pyim.el

| k    | b                            |
| ---- | ---------------------------- |
| M-j  | pyim-convert-string-at-point |

### init-epaint.el

[paint tool.](https://github.com/chuntaro/epaint)

### init-games.el

tetris 俄罗斯方块

| k     | b                  |
| ----- | ------------------ |
| C-p   | tetris-rotate-prev |
| C-n   | tetris-rotate-down |
| C-b   | tetris-move-left   |
| C-f   | tetris-move-right  |
| C-SPC | tetris-move-bottom |

### init-general.el

[define keybinds.](https://github.com/noctuid/general.el)

**my-space-leader-def**

**my-javascript-leader-def**

**my-comma-leader-def**

**my-semicolon-leader-def**

## Site-eslip

### snails

`M-x snails` to launch snails

`M-x snails-search-point` to launch snails with symbol around point.

Snails is a modern, easy-to-expand fuzzy search framework.

https://github.com/manateelazycat/snails/tree/11700b398813f35b4602d5e95391f28efeb4779e

| Key         | Description               |
| ----------- | ------------------------- |
| C-n         | Select next candidate     |
| C-p         | Select previous candidate |
| M-n         | Select next candidate     |
| M-p         | Select previous candidate |
| M-,         | Select next candidate     |
| M-.         | Select previous candidate |
| C-v         | Select next backend       |
| M-v         | Select previous backend   |
| M-j         | Select next backend       |
| M-k         | Select previous backend   |
| C-m         | Confirm candiate          |
| RET         | Confirm candiate          |
| M-w         | Copy candidate            |
| C-g         | Quit snails               |
| ESC ESC ESC | Quit snails               |
| M-h         | Quit snails               |
| C-a`        |                           |

### color-rg(搜索)

https://github.com/manateelazycat/color-rg.git

https://github.com/manateelazycat/color-rg/tree/22b050fc0b9b8d13f10c5fbd4cf14980d2831dc8

**Keymap for view mode**

| Key      | Description                                                  |
| -------- | ------------------------------------------------------------ |
| C-a      | Jump to first editable position of current line              |
| Tab      | Jump to next match keyword                                   |
| Back Tab | Jump to previous match keyword                               |
| j        | Jump to next match keyword                                   |
| k        | Jump to previous match keyword                               |
| h        | Jump to next match file                                      |
| l        | Jump to previous match file                                  |
| RET      | Open file relative to match line                             |
| Ctrl + m | Open file relative to match line                             |
| r        | Replace all matches                                          |
| f        | Filter results match regexp                                  |
| F        | Filter results not match regexp                              |
| x        | Filter results match file extension                          |
| X        | Filter results not match file extension                      |
| u        | Don't filter file extension                                  |
| D        | Delete current line from results                             |
| i        | Toggle to include or exclude the ignore files                |
| t        | Re-search pattern as literal                                 |
| c        | Toggle to smart case or case sensitive                       |
| s        | Re-search with new keyword and default argument              |
| d        | Re-search with new directory                                 |
| z        | Re-search with new files                                     |
| Z        | Rerun last search but prompt for new files which will NOT be searched. |
| C        | Rerun rg with customized arguments.                          |
| e        | Enable edit mode                                             |
| q        | Quit                                                         |

**Keymap for edit mode**

| Key       | Description                                     |
| --------- | ----------------------------------------------- |
| C-a       | Jump to first editable position of current line |
| C-c C-j   | Jump to next match keyword                      |
| C-c C-k   | Jump to previous match keyword                  |
| C-c C-h   | Jump to next match file                         |
| C-c C-l   | Jump to previous match file                     |
| C-c C-RET | Open file relative to match line                |
| C-c C-v   | Disable edit mode                               |
| C-c C-d   | Delete current line                             |
| C-c C-D   | Delete all lines                                |
| C-c C-r   | Recover current line                            |
| C-c C-R   | Recover buffer content                          |
| C-c C-q   | Quit                                            |
| C-c C-c   | Apply changed line to files                     |

## Ivy

### bindings

| key       | function                     |
| --------- | ---------------------------- |
| `C-s`     | swiper-isearch               |
| `C-z s`   | counsel-rg                   |
| `C-z b`   | counsel-buffer-or-recentf    |
| `C-z C-b` | counsel-ibuffer              |
| `C-r`     | ivy-previous-line-or-history |
| `M-RET`   | ivy-immediate-done           |
| `C-~`     | counsel-goto-local-home      |
| `C-M-s`   | color-rg-search-input        |

# bindings

## evil

| name | Function           |
| ---- | ------------------ |
| C-]  | evil-jump-to-tag   |
| C-o  | evil-jump-backward |



## global bindings
| name      | Function                      |
| --------- | ----------------------------- |
| `C-x C-l` | toggle-truncate-lines         |
| `C-+`     | text-scale-increase           |
| `C--`     | text-scale-decrease           |
| `C-z w`   | resize width                  |
| `C-z h`   | resize height                 |
| `M-n`     | forward-paragraph             |
| `M-p`     | backward-paragraph            |
| `M-W =`   | Bigger width                  |
| `M-W M-+` | Bigger width                  |
| `M-W -`   | smaller width                 |
| `M-W M-_` | smaller width                 |
| `M-H =`   | Bigger height                 |
| `M-H M-+` | bigger height                 |
| `M-H -`   | smaller height                |
| `M-H M-_` | smaller height                |
| C-z o     | ffip                          |
| `C-h C-m` | discover-my-major             |
| `C-x C-o` | ace-window                    |
| `M-~`     | shell-here                    |
| `M-#`     | aweshell-dedicated-close/open |
| `M-$`     | multi-term, fullscreen        |

