;; -----------------------------------------------------------------------------
;; Package Management:

;; Cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; Pallet
(require 'pallet)
(pallet-mode t)

;; -----------------------------------------------------------------------------
;; Emacs Settings:

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Font
(set-default-font "Inconsolata-18")

;; Theme
(load-theme 'zenburn t)

;; Display Cursor Position
(setq line-number-mode t)
(setq column-number-mode t)

;; -----------------------------------------------------------------------------
;; Misc:

;; Exec path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
