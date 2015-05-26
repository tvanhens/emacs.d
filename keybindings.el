;; -- Key Chords ---------------------------------------------

(key-chord-define-global "jj" 'hydra-window/body)
(key-chord-define-global "jg" 'magit-status)

;; -- Key Bindings -------------------------------------------

;; Helm

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-i") 'helm-company)

;; Paredit

(global-set-key (kbd "C-k") 'sp-kill-hybrid-sexp)
