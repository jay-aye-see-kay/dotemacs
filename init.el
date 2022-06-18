;; -*- lexical-binding: t; -*-

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:

;; M-x all-the-icons-install-fonts

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; visual stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-default 'truncate-lines t)

;; theme
(setq modus-themes-mode-line '(accented 3d padded)
      x-underline-at-descent-line t
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (overline background 1.4))
        (2 . (background 1.3))
        (3 . (bold 1.2))
        (t . (semilight 1.1))))
;; Load the light theme by default
(load-theme 'modus-operandi t)

;; base font setup 
(setq font-use-system-font t)
(set-face-attribute 'default nil :height 110)
(recentf-mode t)

(use-package all-the-icons)

(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; modes with variable width font (docs + help)
(dolist (mode '(help-mode-hook
		helpful-mode-hook))
  (add-hook mode (lambda () (variable-pitch-mode))))

(use-package dashboard
  :ensure t
  :config
  (setq
   dashboard-startup-banner 'logo
   dashboard-banner-logo-title nil
   dashboard-set-footer nil
   dashboard-items '((projects  . 5)
                     (recents . 5)
                     (agenda . 5))
   ;; show dashboard in new frames (i.e. when emacs server already running)
   initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package nix-mode
  :mode "\\.nix\\'")


;; START completions

;; replacing with project.el....
;; =C-x p p= to open project switcher
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path '("nixfiles" "~/code" "~/code/msh"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package marginalia
  :config
  (marginalia-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; END completions

(load-file "~/.emacs.d/evil.el")
(load-file "~/.emacs.d/keymaps.el")
(load-file "~/.emacs.d/lsp.el")
(load-file "~/.emacs.d/org.el")
(put 'scroll-left 'disabled nil)
