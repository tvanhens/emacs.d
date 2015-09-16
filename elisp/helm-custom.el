(provide 'helm-custom)

(require 'helm)
(require 'helm-config)
(require 'magit)
(require 'company)
(require 'clojure-mode)

(bind-key "C-c h" 'helm-command-prefix)
(unbind-key "C-x c")

(bind-key "<tab>" 'helm-execute-persistent-action)
(bind-key "<tab>" 'magit-section-toggle magit-mode-map)
(bind-key "<tab>" 'company-complete clojure-mode-map)
(bind-key "C-i" 'helm-execute-persistent-action)
(bind-key "C-z" 'helm-select-action)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x b" 'helm-mini)
(bind-key "M-y" 'helm-show-kill-ring)
(bind-key "M-x" 'helm-M-x)
(bind-key "C-c h g" 'helm-google-suggest)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
