(defun self/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . self/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; TODO set this up so it's =SPC l=
  :config
  (lsp-enable-which-key-integration t)
  :custom
  ;; TODO why isn't this working?? https://github.com/emacs-lsp/lsp-mode/issues/1223#issuecomment-586674535
  (lsp-signature-auto-activate nil))

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

;; Langauge configuration ------------------------------------------------------
(use-package typescript-mode
  :mode "\\.ts\\'\\|\\.tsx\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

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
