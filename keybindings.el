;; -- Key Chords ---------------------------------------------------------------

(key-chord-define-global "jw" 'hydra-window/body)
(key-chord-define-global "jg" 'magit-status)
(key-chord-define-global "jc" 'hydra-emacs-config/body)
(key-chord-define-global "jp" 'prodigy)

;; -- Key Bindings -------------------------------------------------------------

;; Helm

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-i") 'helm-company)

;; Paredit

(global-set-key (kbd "C-k") 'sp-kill-hybrid-sexp)
