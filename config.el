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
