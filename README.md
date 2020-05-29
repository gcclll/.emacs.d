# .emacs.d

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
|             |                           |

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



# Self-funcs

| name                 | Keybinding | desc          |
| -------------------- | ---------- | ------------- |
| resize-window-width  | C-z w      | resize width  |
| resize-window-height | C-z h      | resize height |



# bindings
## global bindings
| name      | Function              |
| --------- | --------------------- |
| `C-x C-l` | toggle-truncate-lines |
| `C-+`     | text-scale-increase   |
| `C--`     | text-scale-decrease   |
| `M-n`     | forward-paragraph     |
| `M-p`     | backward-paragraph    |
| `M-W =`   | Bigger width          |
| `M-W M-+` | Bigger width          |
| `M-W -`   | smaller width         |
| `M-W M-_` | smaller width         |
| `M-H =`   | Bigger height         |
| `M-H M-+` | bigger height         |
| `M-H -`   | smaller height        |
| `M-H M-_` | smaller height        |
| C-z o     | ffip                  |

