;; Emacs
(line-number-mode t)
(column-number-mode t)
(load-theme 'monokai t)

;; Company mode
(global-company-mode t)

;; Hungy delete
(global-hungry-delete-mode t)

;; Undo tree
(global-undo-tree-mode t)

;; Smartparens
(smartparens-global-strict-mode t)
(sp-pair "'" nil :actions :rem)

;; Projectile
(projectile-global-mode t)
(helm-projectile-on)

;; Clojure Refactor
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               ))

;; Cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t)

;; Git Gutter
(global-git-gutter-mode +1)
