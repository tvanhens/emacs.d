(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

;; TODO:
;; - change bindings to use bind-key
;; - move keybindings to file
;; - move hydras to file

;; Exec path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Keep cask in sync with package managment
(require 'pallet)

(pallet-mode t)

;; Set theme
(load-theme 'zenburn t)

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Enable helm mode
(helm-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Smart Parens
(require 'smartparens-config)
(smartparens-global-mode 1)

;; Hydras

(require 'windmove)

 (defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; TODO: refactor key-chords into a bindings ns

(require 'key-chord)

(key-chord-define-global "jj" 'hydra-window/body)
(key-chord-define-global "jg" 'magit-status)

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

(key-chord-mode +1)

;; Magit config
(setq magit-last-seen-setup-instructions "1.4.0")
