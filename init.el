;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

(defun jdr/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jdr/display-startup-time)

;; visual stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-default 'truncate-lines t)

;; base font setup
(setq font-use-system-font t)
(set-face-attribute 'default nil :height 110)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Automatically tangle our Emacs.org config file when we save it
(defun jdr/org-babel-tangle ()
  (when (string-equal (file-name-directory buffer-file-name)
                      (file-name-directory user-init-file))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jdr/org-babel-tangle)))

(setq modus-themes-bold-constructs t
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

(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:

;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(recentf-mode t)
(setq recentf-max-saved-items 50)

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  :config
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

;; modes with variable width font (docs + help)
(dolist (mode '(help-mode-hook
                helpful-mode-hook))
  (add-hook mode (lambda () (variable-pitch-mode))))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;

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

(use-package evil
  :ensure t
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Since I let evil-mode take over C-u for buffer scrolling, I need to re-bind
;; the universal-argument command to another key sequence
(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode))

(evil-define-key 'normal 'global (kbd "C-w C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer rune/quick-keys
    :keymaps '(normal)
    :prefix ","))

(rune/leader-keys
  "gs" 'magit-status
  "hf" 'helpful-function
  "hv" 'helpful-variable
  "hk" 'helpful-key)

;; setup avy like my hop.nvim setup
(use-package avy
  :after evil
  :config
  (evil-define-key 'normal 'global "s" 'evil-avy-goto-char)
)

(use-package deadgrep)

;; quick keymaps from vim
(evil-define-key 'normal 'global ",b" 'switch-to-buffer)
(evil-define-key 'normal 'global ",f" 'find-file)
(evil-define-key 'normal 'global ",o" 'recentf-open-files)
(evil-define-key 'normal 'global ",a" 'deadgrep)
(evil-define-key 'normal 'global ",x" 'execute-extended-command)

(defun jdr/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  ;; override variable pitch fonts selectively
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  )

(use-package org
  :hook (org-mode . jdr/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files '("~/Documents/org/" "~/Documents/org/logbook"))
  (setq org-archive-location "~/Documents/org/archive")

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@/!)"
           "|" "DONE(d!)" "CANCELLED(c!)"
           )))
  )

(with-eval-after-load 'org
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)
     (python . t)
     (plantuml . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src js")))

(defun jdr/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jdr/org-mode-visual-fill))

(defun jdr/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jdr/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; TODO set this up so it's =SPC l=
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-symbol t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil))



(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (define-key evil-normal-state-map (kbd "gh") 'lsp-ui-doc-show)
  :custom
  (lsp-ui-doc-position 'top)) ;; can be top, bottom, or at-point


;; Company mode configuration ------------------------------------------------------
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; lsp leader keys
;; (rune/leader-keys
;;     "la" '(projectile--find-file :which-key "code action"))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

(use-package typescript-mode
  :mode "\\.ts\\'\\|\\.tsx\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package plantuml-mode)
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq org-plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)
(setq org-plantuml-exec-mode 'executable)

(use-package magit)

(use-package vterm
  :custom
  (vterm-environment
   '("fish_term24bit=1"
     "fish_color_autosuggestion=#505050"
     "fish_color_cancel=#a60000"
     "fish_color_command=#5317ac"
     "fish_color_comment=#505050"
     "fish_color_cwd=#000000"
     "fish_color_cwd_root=#000000"
     "fish_color_end=#000000"
     "fish_color_error=#a60000"
     "fish_color_escape=#000000"
     "fish_color_history_current=#000000"
     "fish_color_host=#000000"
     "fish_color_host_remote=#000000"
     "fish_color_match=#000000"
     "fish_color_normal=#000000"
     "fish_color_operator=#721045"
     "fish_color_param=#000000"
     "fish_color_quote=#2544bb"
     "fish_color_redirection=#721045"
     "fish_color_search_match=#000000"
     "fish_color_selection=#000000"
     "fish_color_status=#000000"
     "fish_color_user=#000000")))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
)
