;; -- Key Chords ---------------------------------------------------------------

(key-chord-define-global "jw" 'hydra-window/body)
(key-chord-define-global "jg" 'magit-status)
(key-chord-define-global "jc" 'hydra-emacs-config/body)
(key-chord-define-global "jp" 'prodigy)

;; -- Key Bindings -------------------------------------------------------------

;; Helm

(require 'clojure-mode)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-i") 'helm-company)
(bind-key "C-c r" 'cljr-helm clojure-mode-map)
(bind-key* "C-s" 'helm-occur-from-isearch)

;; Paredit

(global-set-key (kbd "C-k") 'sp-kill-hybrid-sexp)
(bind-key "C-)" 'sp-forward-slurp-sexp)
(bind-key "C-c n" 'sp-indent-defun)
(bind-key "M-s" 'sp-splice-sexp)

;; Emacs

(bind-key* "C-a" 'back-to-indentation)
(bind-key* "<s-backspace>" 'hungry-delete-backward)
(bind-key* "C-M-o" 'next-multiframe-window)
(bind-key* "C-M-S-o" 'previous-multiframe-window)
