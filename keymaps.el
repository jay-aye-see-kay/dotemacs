;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Since I let evil-mode take over C-u for buffer scrolling, I need to re-bind
;; the universal-argument command to another key sequence
(global-set-key (kbd "C-M-u") 'universal-argument)
;; stop C-<direction> keys doing weird stuff
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-k") nil)
(global-set-key (kbd "C-l") nil)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; make horizontal touchpad scrolling work (not tested on mac)
(global-set-key (kbd "<triple-wheel-right>")
  (lambda () (interactive) (scroll-right 2)))
(global-set-key (kbd "<triple-wheel-left>")
  (lambda () (interactive) (scroll-left 2)))

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


(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "gs" 'magit-status
  )

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; setup avy like my hop.nvim setup
(use-package avy)
(evil-define-key 'normal 'global "s" 'evil-avy-goto-char)

;; quick keymaps from vim
(evil-define-key 'normal 'global ",b" 'switch-to-buffer)
(evil-define-key 'normal 'global ",f" 'find-file)
(evil-define-key 'normal 'global ",o" 'recentf-open-files)
(evil-define-key 'normal 'global ",a" 'deadgrep)
