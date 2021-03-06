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

;; Display custom keybindings
(bind-key "C-h M-b" 'describe-personal-keybindings)

;; -----------------------------------------------------------------------------
;; Emacs Settings:

;; Disable auto-save
(setq auto-save-default nil)

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

;; Projectile
(projectile-global-mode)

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

;; Org Mode
(setq org-return-follows-link t)

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

;; Clojure Mode Custom Indentations
(define-clojure-indent
  (om/set-state! 'defun)
  (om/transact! 'defun)
  (om/build 'defun)
  (dom/form 'defun)
  (dom/div 'defun))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-use-backtracking-indent t)
 '(helm-external-programs-associations (quote (("html" . "open"))))
 '(safe-local-variable-values
   (quote
    ((eval setenv "DATOMIC_PASSWORD" "1aa95b8f-dda6-4877-93e5-6ed53774cecc")
     (eval setenv "DATOMIC_USERNAME" "tyler@mainstreetgenome.com")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
