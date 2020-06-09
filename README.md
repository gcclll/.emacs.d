# .emacs.d

all copy from https://github.com/MatthewZMD/.emacs.d

## TODOs

1. **TODO** restclient configurations
2. https://github.com/rememberYou/.emacs.d/blob/master/config.org
3. https://github.com/qutebrowser/qutebrowser/blob/master/doc/install.asciidoc

# My Functions

| function    | description                                       |
| ----------- | ------------------------------------------------- |
| `gcl/bakup` | execute `~/.gclrc/run.py` to bakup files on MacOS |



## My keybindings

### **C**(Control)

| key     | function                    |
| ------- | --------------------------- |
| `+`     | text-scale-increase         |
| `-`     | text-scale-decrease         |
| `k`     | crux-smart-kill-line        |
| `e`     | evil-end-of-line            |
| `c h r` | hydra-rectangle/body        |
| `c h b` | hydra-buffer-menu/body      |
| `c h c` | hydra-multiple-cursors/body |
| `c h g` | hydra-ranger/body           |
| `c h o` | hydra-org-agenda/body       |
| `c h q` | hydra-query/body            |

### **M**(alt, option)

| key      | function       |
| -------- | -------------- |
| `M-u`    | uppercase      |
| `M-l`    | lowercase      |
| `M-c`    | capitalize     |
| `M-k`    | sp-kill-sexp   |
| `M-up`   | move-text-up   |
| `M-down` | move-text-down |

### **SPC**

| key   | function    |
| ----- | ----------- |
| `SPC` | counsel-M-x |

**a**

| key  | function                 |
| ---- | ------------------------ |
| `a~` | shell-here               |
| `a3` | aweshell-dedicated-open  |
| `a#` | aweshell-dedicated-close |
| `a$` | multi-term               |
| `ar` | ranger                   |

**b**

| key   | function                  |
| ----- | ------------------------- |
| `bb`  | counsel-switch-buffer     |
| `bd`  | kill-this-buffer          |
| `bi`  | ibuffer                   |
| `bf`  | gcl/open-in-finder        |
| `bK`  | crux-kill-other-buffers   |
| `bo`  | crux-open-with            |
| ` bp` | previous-buffer           |
| `bn`  | next-buffer               |
| `br`  | counsel-buffer-or-recentf |
| `bs`  | save-buffer               |
| `bS`  | save-all-buffers          |
| `bm`  | buffer-menu               |

**c**

| key  | function                              |
| ---- | ------------------------------------- |
| `cp` | crux-duplicate-current-line-or-region |



**d**

| key  | function         |
| ---- | ---------------- |
| `do` | `open-dashboard` |
| `dd` | dash-at-point    |

**f**

| key  | function                    |
| ---- | --------------------------- |
| `ff` | counsel-find-file           |
| `fp` | ffip                        |
| `fr` | crux-recentf-find-file      |
| `fR` | crux-rename-file-and-buffer |
| `fd` | crux-delete-file-and-buffer |

**g**

| key  | function     |
| ---- | ------------ |
| `gg` | magit-status |
| `gb` | gcl/bakup    |
| `gp` | gcl/git-push |

**h**

| key  | function                    |
| ---- | --------------------------- |
| `hr` | hydra-rectangle/body        |
| `hm` | hydra-multiple-cursors/body |
| `ho` | hydra-org-agenda/body       |

**i**

| key  | function             |
| ---- | -------------------- |
| `ie` | emojify-insert-emoji |

**o**

| key  | function     |
| ---- | ------------ |
| `om` | lsp-ui-imenu |

**q**

| key  | function      |
| ---- | ------------- |
| `qr` | restart-emacs |

**r**

| key  | function           |
| ---- | ------------------ |
| `rt` | instant-rename-tag |

**s**

| key  | function             |
| ---- | -------------------- |
| `ss` | swiper               |
| `sS` | swiper-all           |
| `sr` | counsel-rg           |
| `si` | swiper-isearch       |
| `sg` | counsel-git-grep     |
| `sp` | rg-project           |
| `sq` | query-replace        |
| `sQ` | query-replace-regexp |

**w**

| key  | function                  |
| ---- | ------------------------- |
| `wv` | split-window-horizontally |
| `w-` | split-window-vertically   |
| `wl` | evil-window-right         |
| `wL` | crux-transpose-windows    |
| `wH` | crux-transpose-windows    |
| `wh` | evil-window-left          |
| `wk` | evil-window-up            |
| `wj` | evil-window-down          |
| `wd` | delete-window             |
| `wm` | delete-other-windows      |

**y**

| key  | function           |
| ---- | ------------------ |
| `yi` | yas-insert-snippet |
| `yn` | yas-new-snippet    |

### ,(comma)

**g**

| key  | function                                  |
| ---- | ----------------------------------------- |
| `go` | dumb-jump-go-other-window                 |
| `gx` | dumb-jump-go-prefer-external              |
| `gz` | dumb-jump-go-prefer-external-other-window |
| `sb` | engine/search-baidu                       |
| `sd` | engine/search-duckduckgo                  |
| `sg` | engine/search-github                      |
| `si` | engine/search-google-images               |
| `sm` | engine/search-google-maps                 |
| `sr` | engine/search-rfcs                        |
| `ss` | engine/search-amazon                      |
| `st` | engine/search-twitter                     |
| `sy` | engine/search-youtube                     |
| `sw` | engine/search-wikipedia                   |
| `sB` | engine/search-books                       |
| `sG` | engine/search-google                      |
| `sM` | engine/search-melpa                       |
| `sS` | engine/search-stack-overflow              |

### ;(semicolon)

**symbols**

| key  | function        |
| ---- | --------------- |
| `1`  | avy-goto-word-0 |
| `2`  | avy-goto-word-1 |
| `3`  | avy-goto-line   |
| `    | avy-next        |

### `(backquote)

used for single key bindings.

| key  | function            |
| ---- | ------------------- |
| `    | evil-goto-mark      |
| `b`  | dumb-jump-back      |
| `g`  | dumb-jump-go        |
| `j`  | dumb-jump-go-prompt |



# Languages

## Dart

```lisp
(use-package dart-mode
  :defer 0.72
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "/opt/dart-sdk/bin/")
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-c" . flutter-run-or-hot-reload))
  :custom (flutter-sdk-path "/opt/flutter/bin/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :config (flutter-l10n-flycheck-setup))

```





## Vue

https://azzamsa.com/blog/vue-emacs/

http://quanweili.com/2018/03/01/emacs-lsp-vue.html

[Error: Cannot find module 'vscode-css-languageservice/lib/umd/data/browsers'](https://github.com/neoclide/coc-vetur/issues/28)



1. lsp-mode
2. install: `npm install -g vue-language-server`

## Python

### lint configuration

```
(add-hook 'after-init-hook #'global-flycheck-mode)
```

`pip install pylint`

`npm install eslint`

### code format

[black](https://github.com/psf/black)

[configuration reference](https://github.com/wbolster/emacs-python-black/issues/5)

### Jupyter

[TODO](https://realpython.com/emacs-the-best-python-editor/)

[jupyter](https://realpython.com/jupyter-notebook-introduction/)

` pip3 install jupyter`

# Apps

## trizen

https://github.com/trizen/trizen

## browser([qutebrowser](https://github.com/qutebrowser/qutebrowser/blob/master/doc/install.asciidoc))

[pdf.js](https://github.com/mozilla/pdf.js/)

https://mozilla.github.io/pdf.js/getting_started/#download

download pdfjs into `/Users/simon/Library/Application Support/qutebrowser` 

then rename `pdfjs-2.4.456-dist` to `pdfjs`.

### configuration

>http://claude-ray.com/2020/01/01/from-vimium-to-qutebrowser/
>
>qutebrowser 的配置管理十分方便，支持通过修改文件自定配置。
>
>不仅可以用 yml 文件做基础定义，还能使用 python 满足更多的定制需要。因而更推崇直接使用 config.py 做配置管理，Linux 平台在 `~/.config/qutebrowser/config.py`，Mac `~/.qutebrowser/config.py`， Windows 是 `%APPDATA%/qutebrowser/config/config.py`。
>
>如果一开始配置文件不存在，可执行 `:config-write-py` 初始化。另有可选参数 `--force`，强制用当前配置覆写磁盘文件。



## mu4e

[Installation](https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html)

[Github](https://github.com/djcb/mu)

[Configuration](https://www.djcbsoftware.nl/code/mu/mu4e/Minimal-configuration.html)

Needs: xapian, gmime

`mu init -m ~/Maildir --my-address gccll.love@gmail.com`

### install 

1. [mbsync](https://rakhim.org/2020/01/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/)

# Configurations

**maximize frame when startup**

`(add-hook 'window-setup-hook 'toggle-frame-maximized t)`



# Operations

## Rectangle Commands

http://ergoemacs.org/emacs/emacs_string-rectangle_ascii-art.html

### kill-rectangle

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

2. dbus

   ```shell
   $ brew edit emacs
   # make the changes you want
   $ brew install -s emacs
   # source: https://ftp.gnu.org/gnu/emacs/emacs-26.1.tar.gz
   ```

   

# Packages

## <font color="red">TODOs</font>

1. [restclient](https://github.com/pashky/restclient.el)

# [ranger](https://github.com/ralesi/ranger.el)



## keybinds

### **Basic Commands**

| Key Binding | Description                           |
| ----------- | ------------------------------------- |
| `?`         | show basic commands in the minibuffer |
| `du`        | show selected files, file size        |
| `q` / `ZZ`  | close tab or disable (quit) ranger    |
| `Q` / `ZQ`  | disable (quit) ranger                 |
| `C-r`       | refresh ranger buffer                 |

### **Navigation**

| Key Binding       | Description                         |
| ----------------- | ----------------------------------- |
| `k`/`up`          | up                                  |
| `j`/`down`        | down                                |
| `h`/`-`/`left`    | to parent directory                 |
| `l`/`RET`/`right` | to selected directory / open file   |
| `gg`              | to top                              |
| `G`               | to bottom                           |
| `<`               | to previous directory               |
| `>`               | to next directory                   |
| `gh`              | to home directory                   |
| `C-b`             | scroll ranger window full page up   |
| `C-f`             | scroll ranger window full page down |
| `K`/`C-u`         | scroll ranger window half page up   |
| `J`/`C-d`         | scroll ranger window half page down |
| `C-k`             | scroll preview window up            |
| `C-j`             | scroll preview window down          |
| `[`               | previous parent directory           |
| `]`               | next parent directory               |
| `;M-{`            | previous marked file / directory    |
| `;M-}`            | next marked file / directory        |

### **Search for a File / Directory**

| Key Binding | Description                 |
| ----------- | --------------------------- |
| `f`         | search for file / directory |

### **Copy, Rename/Move or Delete**

| Key Binding | Description                                         |
| ----------- | --------------------------------------------------- |
| `;C`        | copy the current/marked file(s) / dir(s)k           |
| `R`         | rename/move the current/marked file(s) / dir(s)     |
| `D`         | delete the current/marked file(s) / dir(s)          |
| `;d`        | flag current file/dir for deletion, and select next |
| `;x`        | delete flagged file(s) / dir(s)                     |

### **Subdirectories**

| Key Binding | Description                              |
| ----------- | ---------------------------------------- |
| `I`         | insert subdirectory from selected folder |
| `gk`        | move to prev subdirectory                |
| `gj`        | move to next subdirectory                |

### **Marking**

| Key Binding        | Description                                |
| ------------------ | ------------------------------------------ |
| `t`                | toggle mark on current file / directory    |
| `;m`/`C-SPC`/`TAB` | mark current file / dir, and select next   |
| `;u`               | unmark current file / dir, and select next |
| `;U`               | unmark all files                           |
| `v`                | invert all marks                           |
| `"`                | mark files (regexp)                        |

### **Sorting**

| Key Binding | Description                    |
| ----------- | ------------------------------ |
| `on`        | name                           |
| `oN`        | name (reverse)                 |
| `oe`        | extension                      |
| `oE`        | extension (reverse)            |
| `os`        | size                           |
| `oS`        | size (reverse)                 |
| `ot`        | modified date/time             |
| `oT`        | modified date/time (reverse)   |
| `oc`        | created date/time              |
| `oC`        | created date/time (reverse)    |
| `zd`        | sort directories first or last |

### **Toggles / Settings**

| Key Binding | Description                            |
| ----------- | -------------------------------------- |
| `i`         | toggle the preview window              |
| `zh`        | toggle hidden files (e.g. dotfiles)    |
| `zi`        | toggle literal / full-text previews    |
| `zf`        | toggle image full-size / fit to window |
| `zP`        | toggle between full ranger / deer-mode |
| `zp`        | toggle file details in deer-mode       |
| `z-`        | reduce number of parent windows        |
| `z+`        | increase number of parent windows      |

### **Shell**

| Key Binding | Description                     |
| ----------- | ------------------------------- |
| `!`         | run shell command on file       |
| `;&`        | run async shell command on file |
| `S`         | eshell popup window             |

### **Other**

| Key Binding | Description                               |
| ----------- | ----------------------------------------- |
| `;+`/`+`    | create directory                          |
| `;=`/`=`    | diff: current file and asks for 2nd file  |
| `;g`        | revert current buffer, with file on disk  |
| `;k`        | kill (hide) marked lines, `C-r` to unhide |

### **Bookmark Navigation**

| Key Binding | Description               |
| ----------- | ------------------------- |
| ```/`'`     | go to bookmark            |
| `m`         | set bookmark              |
| `um`        | delete bookmark           |
| `B`         | show the bookmarks prompt |

### **Tab and History Usage**

| Key Binding | Description                 |
| ----------- | --------------------------- |
| `gn`        | create a new tab            |
| `gT`        | go to previous tab          |
| `gt`        | go to next tab              |
| `gc`        | close current tab           |
| `uq`        | restore the last closed tab |
| `zz`        | search through history      |
| `H`         | history back                |
| `L`         | history next                |

### **Copy and Paste Functionality**

| Key Binding | Description                                     |
| ----------- | ----------------------------------------------- |
| `yy`        | mark files to copy                              |
| `dd`        | mark files to move                              |
| `pp`        | paste files in copy ring                        |
| `po`        | paste files in copy ring and overwrite existing |
| `p?`        | show the copy contents                          |

### **Selected File Actions**

| Key Binding | Description                                            |
| ----------- | ------------------------------------------------------ |
| `ws`        | exit ranger and open selected file in horizontal split |
| `wv`        | exit ranger and open selected file in vertical split   |
| `wf`        | exit ranger and open selected file in new frame        |
| `we`        | open the selected file in external app                 |

### 

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

`(setq flycheck-flake8rc "~/.emacs.d/rcs/.flake8")`

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
7. zoom-mode
8. init-shell.el
9. windmove
10. ace-jump-mode
11. [restart-emacs](https://github.com/iqbalansari/restart-emacs)
12. [dash-at-point](https://github.com/stanaka/dash-at-point.git)
    Install failed, install dash-at-point by package-install
13. [**<font color="red">avy</font>**](https://github.com/abo-abo/avy)
14. [crux](https://github.com/bbatsov/crux)
15. [move-text](https://github.com/emacsfodder/move-text)
16. [electric-operator](https://github.com/rememberYou/.emacs.d/blob/master/config.org),  is an emacs minor-mode to automatically add spacing around operators

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

| Shortcut                   | Function         |
| -------------------------- | ---------------- |
| Tab Or C-i                 | Next Item        |
| Shift-Tab                  | Previous Item    |
| Return / Mouse Click / C-m | Open             |
| r                          | Recent files     |
| m                          | Bookmarks        |
| p                          | Projects         |
| a                          | Org-Mode Agenda  |
| e                          | Registers        |
| g                          | Refresh contents |
| {                          | Previous section |
| }                          |                  |

![](http://qiniu.ii6g.com/1590741049.png?imageMogr2/thumbnail/!100p)

### init-projectile.el

need add .gitmodules

| k         | b                                   |
| --------- | ----------------------------------- |
| SPC p f   | counsel-projectile-find-file        |
| SPC p b   | counsel-projectile-switch-to-buffer |
| SPC p p   | counsel-projectile-switch-project   |
| SPC p g   | counsel-projectile-grep             |
| SPC p SPC | open ~/projects/projects.org        |

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

**[lsp-ui](https://emacs-lsp.github.io/lsp-ui/)**

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

1. rjsx-mode
   [https://medium.com/@jerryhsieh/emacs-30-%E7%94%A8-emacs-%E5%AF%AB-react-a8e904c3b111](https://medium.com/@jerryhsieh/emacs-30-用-emacs-寫-react-a8e904c3b111)
   
2. [elpy](https://github.com/jorgenschaefer/elpy), mode for python

   | key       | function                         |
   | --------- | -------------------------------- |
   | `C-c C-c` | elpy-shell-send-region-or-buffer |
   | `C-RET`   |                                  |
   | `C-c C-z` |                                  |
   | `C-c C-d` |                                  |

3. [resetclient.el](https://github.com/pashky/restclient.el)
   [Usage](https://erick.navarro.io/blog/testing-an-api-with-emacs-and-restclient/)

4. php-mode

   [php-language-server](https://github.com/felixfbecker/php-language-server)

   [install by composer](https://getcomposer.org/doc/00-intro.md#installation-linux-unix-macos)

5. js2-refactor

   ```lisp
   (use-package js2-refactor
     :bind (:map js2-mode-map
                 ("C-k" . js2r-kill)
                 ("M-." . nil))
     :hook ((js2-mode . js2-refactor-mode)
            (js2-mode . (lambda ()
                          (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
     :config (js2r-add-keybindings-with-prefix "C-c C-r"))
   ```

   

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

related functions:

| Application Name    | Launch                                                       |
| ------------------- | ------------------------------------------------------------ |
| Browser             | `M-x eaf-open-browser` Search or Goto URL                    |
|                     | `M-x eaf-open-browser-with-history` Search or Goto URL or Goto History |
| HTML Email Renderer | `M-x eaf-open-mail-as-html` in `gnus`, `mu4e`, `notmuch` HTMl Mail |
| PDF Viewer          | `M-x eaf-open` PDF File                                      |
| Video Player        | `M-x eaf-open` Video File                                    |
| Image Viewer        | `M-x eaf-open` Image File                                    |
| Markdown Previewer  | `M-x eaf-open` Markdown File                                 |
| Org Previewer       | `M-x eaf-open` Org File                                      |
| Camera              | `M-x eaf-open-camera`                                        |
| Terminal            | `M-x eaf-open-terminal`                                      |
| File Sender         | `M-x eaf-file-sender-qrcode` or `eaf-file-sender-qrcode-in-dired` |
| File Browser        | `M-x eaf-file-browser-qrcode`                                |
| Airshare            | `M-x eaf-open-airshare`                                      |
| RSS Reader          | `M-x eaf-open-rss-reader`                                    |
| Mindmap             | `M-x eaf-create-mindmap` or `M-x eaf-open-mindmap`           |
| Doc Viewer          | `M-x eaf-open-office`                                        |
| Mermaid             | `M-x eaf-open` Mermaid file (*.mmd)                          |
| Demo                | `M-x eaf-open-demo` to verify basic functionality            |

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

https://freenode.net/

https://blog.csdn.net/houyp520/article/details/9034127

`/msg NickServ VERIFY REGISTER zhicheng lerdusxgfryg`

https://github.com/rememberYou/.emacs.d/blob/master/config.org

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

# init-dydra.el

[use-package-hydra](https://gitlab.com/to1ne/use-package-hydra)

[hydra wiki](https://github.com/abo-abo/hydra/wiki)

[hydra examples](https://github.com/abo-abo/hydra/blob/master/hydra-examples.el)



1. Buffer-menu-mode
2. [hydra-rectangle](https://github.com/abo-abo/hydra/wiki/Rectangle-Operations)

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

