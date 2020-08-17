# keybinds

## C, S(Control, Option/Alt, ...)

| key          | function                                               |
| ------------ | ------------------------------------------------------ |
| `<C-return>` | rect-hydra/body，text-mode, prog-mode,打开快捷操作面板 |
| `<s-return>` | rect-hydra/body, org-mode,打开快捷操作面板             |
| `<C-;>`      | iedit-mode                                             |
| `<C-=>`      | er/expand-region，选择区域                             |
| `<C-,>`      | goto-last-change                                       |



## F1-F12(hydra, ...functional)

| key    | function                                  |
| ------ | ----------------------------------------- |
| `<f6>` | `toggles-hydra/body`，打开 hydra 快捷界面 |

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

### c

| key       | function           |
| --------- | ------------------ |
| `SPC c w` | counsel-colors-web |



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

| key       | function                   |
| --------- | -------------------------- |
| `SPC s a` | swiper-all                 |
| `SPC s i` | swiper-isearch             |
| `SPC s b` | swiper-isearch-backward    |
| `SPC s s` | swiper                     |
| `SPC s r` | ivy-resume，恢复上一次查找 |



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

