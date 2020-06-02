;;; init-hydra.el --- keys manager
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `hydra'
;;----------------------------------------------------------------------------
(use-package hydra
  :config
  (defhydra hydra-zoom (:color red)
    "Zoom"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("/" (text-scale-adjust 0) "Reset"))

  :general
  (:states 'normal
   "C-+" 'hydra-zoom/text-scale-increase
   "C--" 'hydra-zoom/text-scale-decrease
   "C-/" '(lambda () (interactive) (text-scale-adjust 0)))
  (:states '(normal emacs)
   "é" 'hydra-launcher/body))
;; -END

;;----------------------------------------------------------------------------
;; `buffer-menu'
;;----------------------------------------------------------------------------
(defhydra hydra-buffer-menu (:color pink)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
;; -END

;;----------------------------------------------------------------------------
;; `hydra-rectangle'
;;----------------------------------------------------------------------------
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok
;; -END

;;----------------------------------------------------------------------------
;; `hydra-multiple-cursors'
;;----------------------------------------------------------------------------
(defhydra hydra-multiple-cursors (:color blue)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_y_] Yank Rectangle
 [Click] Cursor at point       [_q_] Quit            ^ ^
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("y" yank-rectangle)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))
;; -END

;;----------------------------------------------------------------------------
;; `hydra-ranger'
;;----------------------------------------------------------------------------
(defhydra hydra-ranger (:color pink)
  "
C-current, M-marked, F-flagged file/dir(s), R-reverse, T-toggle

^mark|unmark^         ^copy|paste^                ^sort^                  ^toggle|open^             ^others^               ^shell^
^^^^^^^^------------------------------------------------------------------------------------------------------------------------------
[_;d_]: make flag     [_yy_]: mark copy           [_on_]: name            [_zd_]: first/last        [_B_]: show bookmarks  [_;&_]: async shell CMD
[_;x_]: flag delete   [_dd_]: mark cut            [_oN_]: name            [_zp_]: details           [_f_]: travel          [_!_]: run shell CMD
[_;m_]: mark          [_pp_]: paste               [_oe_]: extension       [_zp_]: dired<->ranger    [_R_]: rename(C|M)     [_S_]: eshell popup
[_;u_]: unmark        [_po_]: paste&overwrite     [_oE_]: extension       [_zh_]: hidden files      [_D_]: delete(C|M)     ^ ^
[_;U_]: unmark All    [_p?_]: show copy contents  [_os_]: size            [_zf_]: file size         [_g_]: go              ^ ^
[_;g_]: revert mark   [_pl_]: symlink             [_oS_]: size            [_zz_]: history           [_/_]: search          ^ ^
[_;k_]: kill lines    [_pL_]: relative symlink    [_ot_]: modified time   [_ws_]: vertically        [_}_]: find file       ^ ^
[_du_]: selected      [_yp_]: absolute path       [_oT_]: modified time   [_wv_]: horizontally      [_C-r_]: refresh       ^ ^
[_;C_]: copy(C|M)     [_yd_]: directory path      [_oc_]: create time     [_wj_]: other window      [_+_]: create dir      ^ ^
[_`_] : goto mark     [_yn_]: filename            [_oC_]: create time     [_we_]: external app      [_=_]: differ          ^ ^
[_t_] : toggle mark   ^ ^                         ^ ^                     ^ ^                       [_q_]: quit            ^ ^
[_v_] : toggle marks  ^ ^                         ^ ^                     ^ ^                       ^ ^                    ^ ^
[_\"_]: mark(regexp)  ^ ^                         ^ ^                     ^ ^                       ^ ^                    ^ ^
[_TAB_]: mark         ^ ^                         ^ ^                     ^ ^                       ^ ^                    ^ ^
"
  ("}" ranger-find-file)
  ("ws" ranger-open-file-vertically)
  ("wv" ranger-open-file-horizontally)
  ("wj" ranger-open-file-other-window)
  ("we" ranger-open-in-external-app)
  ("TAB" ranger-mark)
  (";C" ranger-copy-current-dir-path)
  (";d" dired-flag-file-deletion)
  (";x" dired-do-flagged-delete)
  (";m" ranger-create-mark)
  (";u" dired-unmark)
  (";U" dired-unmark-all-marks)
  (";&" async-shell-command)
  (";+" mkdir)
  ("+" mkdir)
  (";=" dired-diff)
  ("=" dired-diff)
  (";g" nil :color red)
  (";k" nil :color red)
  ("du" ranger-show-size)
  ("C-r" ranger-refresh)
  ("B" ranger-show-bookmarks)
  ("R" dired-do-rename)
  ("D" dired-do-delete)
  ("t" ranger-toggle-mark)
  ("f" ranger-travel)
  ("v" dired-toggle-marks)
  ("\"" dired-mark-files-regexp)
  ("!" dired-do-shell-command)
  ("S" ranger-pop-eshell)
  ("/" ranger-search)
  ("g" ranger-go)
  ("on" dired-sort-name)
  ("oN" dired-sort-name)
  ("os" dired-sort-size)
  ("oS" dired-sort-size)
  ("oe" dired-sort-extension)
  ("oE" dired-sort-extension)
  ("ot" dired-sort-utime)
  ("oT" dired-sort-utime)
  ("oc" dired-sort-time)
  ("oC" dired-sort-time)
  ("zd" ranger-toggle-dir-first)
  ("zp" ranger-to-dired)
  ("zP" ranger-to-dired)
  ("zf" ranger-toggle-scale-images)
  ("zh" ranger-toggle-dotfiles)
  ("zz" ranger-show-history)
  ("yy" ranger-copy)
  ("yp" ranger-copy-absolute-file-paths)
  ("yd" ranger-copy-current-dir-path)
  ("yn" ranger-copy-filename)
  ("dd" ranger-cut)
  ("pp" ranger-paste)
  ("po" ranger-paste-over)
  ("p?" ranger-show-copy-contents)
  ("pl" ranger-paste-as-symlink)
  ("pL" ranger-paste-as-relative-symlink)
  ("`" ranger-goto-mark)
  ("q" nil :color red))

;; -END

;;----------------------------------------------------------------------------
;; `hydra-org-agenda'
;;----------------------------------------------------------------------------
;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))
;; -END

(provide 'init-hydra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
