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

;; Eldoc Mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; -----------------------------------------------------------------------------
;; Package Config:

;; Exec path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Magit
(setq magit-push-always-verify nil)
(bind-key "C-x g" 'magit-status)

;; Smartparens
(require 'smartparens-config)
(smartparens-global-strict-mode t)
