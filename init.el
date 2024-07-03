;; -*- lexical-binding: t -*-

;; Setup elpaca for package management
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Enable use-package :ensure support for Elpaca.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Enable built-in winmove keybindings (e.g. shift + direction for changing windows)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package emacs
  :bind
  (("C-x C-r" . restart-emacs))
  :custom
  (make-backup-files nil)
  ;; Only include commands appropriate for the current buffer
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (set-frame-font "Inconsolata 18" nil t)
  (electric-pair-mode t)
  (recentf-mode t)

  (desktop-save-mode t)

  (setq-default tab-width 4)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; OSX Specific config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(use-package windmove
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(use-package exec-path-from-shell :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package better-defaults :ensure t)

;; (use-package gruvbox-theme :ensure t
;;   :init
;;   (load-theme 'gruvbox-dark-medium t))

(use-package cyberpunk-theme :ensure t
  :init
  (load-theme 'cyberpunk t))

(use-package all-the-icons :ensure t
  :if (display-graphic-p))

(use-package which-key :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package multiple-cursors :ensure t
  :bind (("M-d" . 'mc/mark-next-like-this-symbol)))

(use-package dirvish :ensure t
  :after all-the-icons
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dirvish-default-layout '(0 0.4 0.6))
  
  :bind
  (("C-x d" . dirvish-side)
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("n"   . dirvish-narrow)
   ("TAB" . dirvish-subtree-toggle)))

(use-package dired-gitignore :ensure t
  :init
  (dired-gitignore-global-mode t)
  :bind
  (:map dirvish-mode-map
        ("h" . #'dired-gitignore-global-mode)))

(use-package magit :ensure t :after transient
  :bind (("C-x g" . magit-status)))
(use-package transient :ensure t)

(use-package undo-tree :ensure t
  :bind (("C-x u"))
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package eat :ensure t
  :bind (("C-c C-t" . eat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure t
  ;; Corfu is used for completions
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package vertico
  :ensure t
  :init
  (ido-mode nil)
  (vertico-mode t))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("C-j"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("M-y"   . consult-yank-pop)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-."   . embark-act)         ;; pick some comfortable binding
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun my/run-go-test (content)
    (let ((match-pos (string-match "func[[:space:]]+\\([[:word:]]+\\)" content)))
      (if match-pos
          (let ((fn-name (match-string 1 content)))
            (let ((default-directory (project-root (project-current t))))
              (shell-command (concat "go test -v -run '^" fn-name "$' ./..."))))
        "No match found")))
  
  (keymap-set embark-defun-map "C-t" #'my/run-go-test))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/check-eglot-dep (bin install-command)
  "Check installation of eglot dependency and warn if it doesn't exist."
  (let ((path  (executable-find bin)))
    (if (not path)
        (warn (concat bin " not found. Install with `" install-command "`"))
      t)))

(use-package eglot
  :commands eglot-ensure
  :bind
  (("C-<return>" . eglot-code-actions)))

(use-package prog-mode
  :hook ((prog-mode . display-line-numbers-mode)))

(use-package go-mode :ensure (:wait t)
  :if (my/check-eglot-dep "gopls" "go install golang.org/x/tools/gopls@latest")
  :commands go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)))

(use-package rust-mode :ensure (:wait t)
  :if (my/check-eglot-dep "rust-analyzer" "rustup component add rust-analyzer")
  :commands rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . eglot-ensure)))

(use-package yaml-mode :ensure (:wait t)
  :commands yaml-mode
  :mode "\\.yaml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ML And coding assistants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel :ensure t
  :bind (("C-c C-g m"   . gptel-menu)
         ("C-c C-g RET" . gptel-send)
         ("C-c C-g c"   . gptel)))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)))
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
