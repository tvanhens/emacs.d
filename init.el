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
  (global-display-line-numbers-mode)
  (desktop-save-mode 1)
  (electric-pair-mode t)
  (setq-default tab-width 4)
  (tab-bar-mode t)
  (recentf-mode t)

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

(use-package exec-path-from-shell :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package better-defaults :ensure t)

(use-package gruvbox-theme :ensure t
  :init
  (load-theme 'gruvbox-dark-medium t))

(use-package all-the-icons :ensure t
  :if (display-graphic-p))

(use-package which-key :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package corfu :ensure t
  ;; Corfu is used for completions
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package vertico :ensure t
  :init
  (ido-mode nil)
  (vertico-mode t))

(use-package vertico-directory :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-j" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  (("C-x d" . dirvish)
   :map dirvish-mode-map
   ("a" . dirvish-quick-access)
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

(use-package consult :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("M-y"   . consult-yank-pop)))

(use-package undo-tree :ensure t
  :bind (("C-x u"))
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;; (use-package flycheck :ensure t
;;   :config
;;   (global-flycheck-mode))

;; (use-package projectile :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))

;; (use-package undo-tree :ensure t
;;   :bind (("C-x u"))
;;   :config
;;   (setq undo-tree-auto-save-history nil)
;;   (global-undo-tree-mode))

;; ;; May not be needed in emacs 30


;; (use-package multiple-cursors :ensure t
;;   :bind (("M-d" . 'mc/mark-next-like-this-symbol)))

;; (use-package yasnippet :ensure t)

;; (use-package lsp-mode :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-l")
;;   :hook ((go-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :config
;;   (add-hook 'before-save-hook #'lsp-format-buffer 0 t))
;; (use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; (use-package lsp-ivy :ensure t :after counsel :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; ;; Languages
;; (use-package go-mode :ensure t
;;   :mode "\\.go\\'")
