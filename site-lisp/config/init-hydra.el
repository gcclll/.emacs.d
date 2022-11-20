
(require 'hydra)

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
(defhydra hydra-lsp (:exit t :hint nil)
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

(provide 'init-hydra)
