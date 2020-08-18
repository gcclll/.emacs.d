# keybinds

| key  | function   |
| ---- | ---------- |
| `C`  | Control    |
| `M`  | Option/Alt |
| `s`  | Command    |

## F1-F12(hydra, ...functional)

| key       | function                                  |
| --------- | ----------------------------------------- |
| `<f6>`    | `toggles-hydra/body`，打开 hydra 快捷界面 |
| `<f8>`    | treemacs                                  |
| `<M-f3>`  | symbol-overlay-remove-all                 |
| `<C-f9>`  | bongo，音乐播放器                         |
| `<C-f12>` | open-calendar                             |
| `<s-f12>` | pomidor, 酷炫的计时器                     |

## 

## modes(模式分类按键)

### howdoyou(搜索 stackover及其姐妹网站)

### pomidor(简易计时器)

| Key   | Description             |
| ----- | ----------------------- |
| Enter | Start new pomodoro.     |
| Space | Start a break.          |
| R     | Resets the timer.       |
| q     | Quit pomidor buffer.    |
| Q     | Turns off pomidor.      |
| h     | Put the session on hold |
| H     | Resume on hold session  |

### org-mode

#### keybindings

| key     | function    |
| ------- | ----------- |
| `C-c a` | org-agenda  |
| `C-c b` | org-switchb |

#### <font color="red">TODO</font> org-roam

<span id="key-org-roam"></span>

| key       | function                  |
| --------- | ------------------------- |
| `C-c n l` | org-roam                  |
| `C-c n f` | org-roam-find-file        |
| `C-c n g` | org-roam-graph            |
| `C-c n i` | org-roam-insert           |
| `C-c n I` | org-roam-insert-immediate |



#### < 前缀操作

可以通过按 `<` 弹出 `org-hydra` 来选择要插入的模板，如下：

![](http://qiniu.ii6g.com/1597737748.png?imageMogr2/thumbnail/!100p)





### markdown

预览插件：`pip3 install grip`

### eshell/shell

### treemacs(目录树🌲)

| key                                | function |
| ---------------------------------- | -------- |
| `M-0`                              | treemacs |
| [more key binds ->](#key-treemacs) |          |



### hydra(快捷操作)

| key          | function              |
| ------------ | --------------------- |
| `C-c w`      | ace-window-hydra/body |
| `<C-return>` | rect-hydra/body       |
| <C-`>        | origami-hydra/body    |
| `<f6>`       | toggles-hydra/body    |
| `<C-f6>`     | doom-modeline-hydra   |



### highlight(M-key, 高亮，标记)

| key   | function                                              |
| ----- | ----------------------------------------------------- |
| `M-i` | symbol-overlay-put，标记                              |
| `M-n` | symbol-overlay-jump-next                              |
| `M-p` | symbol-overlay-jump-prev                              |
| `M-N` | symbol-overlay-switch-forward                         |
| `M-P` | symbol-overlay-switch-backward                        |
| `M-C` | symbol-oveRlay-remove-all                             |
| `p`   | symbol-overlay-jump-prev，同类标记的前一个            |
| `n`   | symbol-overlay-jump-next，同类标记的下一个            |
| `>`   | symbol-overlay-jump-last, 跳到当前标记最后一个        |
| `<`   | symbol-overlay-jump-first，跳到当前标记的第一个       |
| `d`   | symbol-overlay-jump-to-definition，跳到变量定义的地方 |

![](http://qiniu.ii6g.com/2020-08-18 11.46.00.gif?imageMogr2/thumbnail/!100p)

### company(补全)

| key                |             | function                         |
| ------------------ | ----------- | -------------------------------- |
| `M-/`              |             | company-complete                 |
| `C-M-i`            |             | company-complete                 |
| company-mode-map   |             |                                  |
|                    | `<backtab>` | company-yasnippet                |
| company-active-map |             |                                  |
|                    | `C-p`       | company-select-prevoius          |
|                    | `C-n`       | company-select-next              |
|                    | `<tab>`     | company-complete-common-or-cycle |
|                    | `<backtab>` | my-company-yasnippet             |
| company-serach-map |             |                                  |
|                    | `C-p`       | company-select-previous          |
|                    | `C-n`       | company-select-next              |



## C, S(Control, Option/Alt, ...)

| key          | function                                               |
| ------------ | ------------------------------------------------------ |
| `<C-return>` | rect-hydra/body，text-mode, prog-mode,打开快捷操作面板 |
| `<s-return>` | rect-hydra/body, org-mode,打开快捷操作面板             |
| `<C-;>`      | iedit-mode                                             |
| `<C-=>`      | er/expand-region，选择区域                             |
| `<C-,>`      | goto-last-change                                       |

### C-c n(org-roam)

[key-bindings ->](#key-org-roam)

## C-c(counsel-xxx)



| key     | function                     |
| ------- | ---------------------------- |
| `C-c B` | counsel-bookmarked-directory |
| `C-c L` | counsel-load-library         |
| `C-c O` | counsel-find-file-extern     |
| `C-c P` | counsel-package              |
| `C-c R` | counsel-list-processes       |
| `C-c f` | counsel-find-library         |
| `C-c g` | counsel-grep                 |

### C-c n(org-roam)

[keybindings->](#key-org-roam)

### C-c c(counsel)

```
// TODO
  ("C-c c B" . counsel-bookmarked-directory)
  ("C-c c F" . counsel-faces)
  ("C-c c L" . counsel-load-library)
  ("C-c c O" . counsel-find-file-extern)
  ("C-c c P" . counsel-package)
  ("C-c c R" . counsel-list-processes)
  ("C-c c a" . counsel-apropos)
  ("C-c c e" . counsel-colors-emacs)
  ("C-c c f" . counsel-find-library)
  ("C-c c g" . counsel-grep)
  ("C-c c h" . counsel-command-history)
  ("C-c c i" . counsel-git)
  ("C-c c j" . counsel-git-grep)
  ("C-c c l" . counsel-locate)
  ("C-c c m" . counsel-minibuffer-history)
  ("C-c c o" . counsel-outline)
  ("C-c c p" . counsel-pt)
  ("C-c c r" . counsel-rg)
  ("C-c c s" . counsel-ag)
  ("C-c c t" . counsel-load-theme)
  ("C-c c u" . counsel-unicode-char)
  ("C-c c w" . counsel-colors-web)
  ("C-c c v" . counsel-set-variable)
  ("C-c c z" . counsel-fzf)
```



## C-s

| key     | function            |
| ------- | ------------------- |
| `C-s-=` | text scale increase |
| `C-s--` | text scale decrease |
| `C-s-0` | text scale reset    |

## ;(Semicolon,分号)

| key  | function                                              |
| ---- | ----------------------------------------------------- |
| `;`  | avy-goto-char                                         |
| `'`  | avy-gooto-char-2                                      |
| `,`  | avy-next                                              |
| `d`  | avy-zap-to-char-dwim，删除光标位置到指定位置的内容    |
| `D`  | avy-zap-up-to-char-dwim，删除光标位置到指定位置的内容 |
| `l`  | avy-goto-line                                         |
| `o`  | ace-link-addr，打开视图中的 url                       |
| `0`  | avy-goto-word-0                                       |
| `1`  | avy-goto-word-1                                       |



## SPC

### a(apps, ...)

| key       | function |
| --------- | -------- |
| `SPC a r` | ranger   |





### b(buffer, ...)

| key       | function                  |
| --------- | ------------------------- |
| `SPC b b` | counsel-switch-buffer     |
| `SPC b d` | kill-this-buffer          |
| `SPC b i` | ibuffer                   |
| `SPC b f` | gcl/open-in-finder        |
| `SPC b K` | crux-kill-other-buffers   |
| `SPC b o` | curx-open-with            |
| `SPC b p` | previous-buffer           |
| `SPC b n` | next-buffer               |
| `SPC b r` | counsel-buffer-or-recentf |
| `SPC b m` | buffer-menu               |



### c(colors, ...)

| key       | function           |
| --------- | ------------------ |
| `SPC c w` | counsel-colors-web |

### d(dash, ...)

| key       | function      |
| --------- | ------------- |
| `SPC d d` | dash-at-point |

### f(file, buffer, ...)

| key       | function                    |
| --------- | --------------------------- |
| `SPC f f` | counsel-find-file           |
| `SPC f e` | crux-find-user-init-file    |
| `SPC f p` | ffip                        |
| `SPC f r` | crux-recentf-find-file      |
| `SPC f R` | crux-rename-file-and-buffer |
| `SPC f d` | crux-delete-file-and-buffer |

### g(git, ...)

| key        | function                   |
| ---------- | -------------------------- |
| `SPC g g`  | magit-status               |
| `SPC g b`  | gcl/bakup, 备份一些文件    |
| `SPC g p ` | gcl/git-push，提交当前仓库 |

### i(insert, emoji, ...)

| key       | function             |
| --------- | -------------------- |
| `SPC i e` | emojify-insert-emoji |
| `SPC i m` | imenu                |





### l(load something)

| key       | function                      |
| --------- | ----------------------------- |
| `SPC l a` | counsel-osx-app，打开app      |
| `SPC l c` | counsel-world-clock，世界时间 |
| `SPC l C` | counsel-colors-web，颜色表    |
| `SPC l s` | counsel-tramp，ssh 列表       |

### m(mode, open mode, ...)

| key       | function             |
| --------- | -------------------- |
| `SPC m i` | iedit-rectangle-mode |



### s(search engine)

| key       | function                           |
| --------- | ---------------------------------- |
| `SPC s a` | swiper-all                         |
| `SPC s b` | swiper-isearch-backward            |
| `SPC s i` | swiper-isearch                     |
| `SPC s g` | counsel-git-grep                   |
| `SPC s p` | rg-project，搜索整个项目           |
| `SPC s q` | query-replace，搜索替换            |
| `SPC s Q` | query-replace-regexp，搜索正则替换 |
| `SPC s r` | ivy-resume，恢复上一次查找         |
| `SPC s R` | counsel-rg，rg 命令搜索            |
| `SPC s s` | swiper                             |
| `SPC s z` | counsel-fzf，fzf 命令搜索          |

### t(treemacs, ...)

<span id="key-treemacs"></span>

| key       | function           |
| --------- | ------------------ |
| `SPC t b` | treemacs-bookmark  |
| `SPC t f` | treemacs-find-file |
| `SPC t t` | treemacs           |
| `SPC t T` | treemacs-find-tag  |



### u(url-related)

| key       | function             |
| --------- | -------------------- |
| `SPC u .` | browse-url-at-point  |
| `SPC u b` | browse-url-of-buffer |
| `SPC u c` | counsel-unicode-char |
| `SPC u r` | browse-url-of-region |
| `spc u u` | browse-url           |
| `SPC u e` | browse-url-emacs     |
| `SPC u f` | browse-url-of-file   |

### v(view, visual, ...)

| key       | function                                               |
| --------- | ------------------------------------------------------ |
| `SPC v p` | ivy-push-view，保存当前视图                            |
| `SPC v o` | ivy-pop-view，查找视图，出栈入栈操作，执行完会删除view |
| `SPC v .` | ivy-switch-view，切换保存的视图                        |

### w(window, ...)

| key       | function                  |
| --------- | ------------------------- |
| `SPC w v` | split-window-horizontally |
| `SPC w -` | split-window-vertically   |
| `SPC w l` | evil-window-right         |
| `SPC w L` | crux-transpose-windows    |
| `SPC w h` | evil-window-left          |
| `SPC w H` | crux-transpose-windows    |
| `SPC w k` | evil-window-up            |
| `SPC w j` | evil-window-down          |
| `SPC w d` | delete-window             |
| `SPC w m` | delete-other-windows      |

### y(yasnippet, ...)

| key       | function           |
| --------- | ------------------ |
| `SPC y i` | yas-insert-snippet |
| `SPC y n` | yas-new-snippet    |

