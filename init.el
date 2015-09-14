;; -----------------------------------------------------------------------------
;; Package Management:

;; Cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; Pallet
(require 'pallet)
(pallet-mode t)

;; Custom packages
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; -----------------------------------------------------------------------------
;; Emacs Settings:

;; Inhibit startup screen
(setq inhibit-startup-message t)

;; Open all in same buffer
(setq ns-pop-up-frames nil)

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

(require 'helm-custom)

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
(bind-key "C-x p" 'smartparens-global-strict-mode)

;; Cider
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Rainbow Delimeters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Whitespace Cleanup
(global-whitespace-cleanup-mode t)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Ace Window
(bind-key "M-p" 'ace-window)
(bind-key "C-x o" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-background nil)

;; -----------------------------------------------------------------------------
;; Customizations:

;; Jump to config
(defun jump-to-init-el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(bind-key "C-c c" 'jump-to-init-el)

;; Indent Buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(bind-key "C-c n" 'indent-buffer)
