;; -- Emacs config -------------------------------------------------------------

(defhydra hydra-emacs-config (:color blue
                              :hint nil)
  ("i" (find-file "~/.emacs.d/init.el") "init")
  ("c" (find-file "~/.emacs.d/config.el") "config")
  ("k" (find-file "~/.emacs.d/keybindings.el") "keys")
  ("h" (find-file "~/.emacs.d/hydra.el") "hydras"))

;; -- Window -------------------------------------------------------------------

(defhydra hydra-window (:color red
                                :hint nil)
   "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("H" hydra-move-splitter-left)
   ("J" hydra-move-splitter-down)
   ("K" hydra-move-splitter-up)
   ("L" hydra-move-splitter-right)
   ("|" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("_" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("v" split-window-right)
   ("x" split-window-below)
                                        ;("t" transpose-frame "'")
   ("u" winner-undo)
   ("r" winner-redo) ;;Fixme, not working?
   ("o" delete-other-windows :exit t)
   ("a" ace-window :exit t)
   ("f" new-frame :exit t)
   ("s" ace-swap-window)
   ("da" ace-delete-window)
   ("dw" delete-window)
   ("db" kill-this-buffer)
   ("df" delete-frame :exit t)
   ("q" nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
   ("b" helm-buffers-list "buf")
   ("m" headlong-bookmark-jump))

