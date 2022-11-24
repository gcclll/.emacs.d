(use-package hydra)

;; --- org-roam
(defhydra hydra-roam (:exit t :columns 3)
  "org-roam
-------------------------------------------------------------------------------------
"
  ("," org-roam-buffer-toggle "Toggle Buffer")
  ("a" org-roam-alias-add "Add Alias")
  ("b" consult-org-roam-backlinks "Backward Links")
  ("c" org-roam-capture "Capture")
  ("C" org-id-get-create "Create ID")
  ("d" org-roam-dailies-capture-today "Capture Dailies")
  ("f" consult-org-roam-file-find "Find File")
  ("g" org-roam-graph "Graph")
  ("i" org-roam-node-insert "Insert Node")
  ("l" consult-org-roam-forward-links "Forward Links")
  ("n" org-roam-node-find "Find Node")
  ("r" org-roam-node-random "Random Node")
  ("s" consult-org-roam-search "Search")
  ("t" org-roam-tag-add "Add Tag")
  ("u" org-roam-ui-open "Roam UI")
  ("q" nil "Quit")
  )

;; --- lsp
(defhydra hydra-lsp-mode (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" format-all-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

;; --- lsp-bridge
(defhydra hydra-lsp-bridge (:exit t :hint nil)
  "
 Find^^                            Diagnostic^^         Other
-------------------------------------------------------------------------------------
 [_d_] definition                  [_n_] next          [_._] restart
 [_D_] definition window           [_p_] previous      [_,_] rename
 [_b_] definition back             [_l_] list          [_h_] sdcv helper
 [_i_] implementation              [_c_] copy
 [_I_] implementation window       [_x_] ignore
 [_r_] references
"
  ("d" lsp-bridge-find-def)
  ("D" lsp-bridge-find-def-other-window)
  ("b" lsp-bridge-find-def-return)
  ("i" lsp-bridge-find-impl)
  ("I" lsp-bridge-find-impl-other-window)
  ("r" lsp-bridge-find-references)

  ("n" lsp-bridge-diagnostic-jump-next)
  ("p" lsp-bridge-diagnostic-jump-prev)
  ("l" lsp-bridge-diagnostic-list)
  ("c" lsp-bridge-diagnostic-copy)
  ("x" lsp-bridge-diagnostic-ignore)

  ("." lsp-bridge-restart-process)
  ("," lsp-bridge-rename)
  ("h" lsp-bridge-toggle-sdcv-helper)

  ("q" nil "quit"))

;; --- smerge
(defhydra hydra-smerge (:color red :hint nil)
  "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("c" smerge-keep-current)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("e" smerge-ediff)
  ("j" previous-line)
  ("k" forward-line)
  ("r" smerge-refine)
  ("u" undo)
  ("q" nil :exit t))

;; --- search
(defhydra hydra-search (:exit t :hint nil)
  "
 color-rg^^                            Diagnostic^^         Other
-------------------------------------------------------------------------------------
 [_i_] Input Project
 [_s_] Symbol Project
 [_t_] File Extension
"
  ("i" color-rg-search-input-in-project)
  ("s" color-rg-search-symbol-in-project)
  ("t" color-rg-search-project-with-type)

  ("q" nil "quit"))

(provide 'init-hydra)
