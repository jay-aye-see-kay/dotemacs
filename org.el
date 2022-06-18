(defun self/org-mode-setup ()
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
  :hook (org-mode . self/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files '("~/Documents/org/" "~/Documents/org/logbook"))
  (setq org-archive-location "~/Documents/org/archive")

  (setq org-todo-keywords
    '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c!)")))
  )
