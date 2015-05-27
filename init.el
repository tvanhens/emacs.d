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

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Enable helm mode
(helm-mode 1)
(helm-autoresize-mode 1)

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
(key-chord-mode +1)
 
;; Magit config
(setq magit-last-seen-setup-instructions "1.4.0")

;; Load modules
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/hydra.el")
