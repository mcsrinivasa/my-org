;;; org-emacs.el --- General Emacs quality-of-life settings
;;; Commentary:
;; This file contains UI tweaks, editing comfort settings, file handling improvements,
;; and discoverability helpers, tuned for Org-mode heavy workflows.

;;; Code:

;; ------------------------------------------------------------
;; 🖥 Interface Tweaks
;; ------------------------------------------------------------

;; Disable GUI chrome for a cleaner look
(menu-bar-mode -1)    ; no menu bar
(tool-bar-mode -1)    ; no tool bar
(scroll-bar-mode -1)  ; no scroll bar

;; Show absolute, global line numbers
(global-display-line-numbers-mode 1)

;; Show column number in mode line
(column-number-mode 1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Auto-pair brackets and quotes
(electric-pair-mode 1)

;; Highlight current line for better focus
(global-hl-line-mode 1)

;; Better scrolling behavior
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Smooth mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5))
      mouse-wheel-progressive-speed nil)

;; ------------------------------------------------------------
;; 💾 File Handling
;; ------------------------------------------------------------

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)

;; Remember cursor position in files
(save-place-mode 1)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Centralize backups and auto-saves
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(setq make-backup-files nil) ; disable backups entirely if desired

;; Silence the bell
(setq ring-bell-function 'ignore)

;; ------------------------------------------------------------
;; ✍️ Editing Comfort
;; ------------------------------------------------------------

;; Delete selection when typing
(delete-selection-mode 1)

;; Show trailing whitespace in programming modes
(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

;; Default fill column and no tabs
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

;; Auto-fill in text modes
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; ------------------------------------------------------------
;; 🔍 Discoverability
;; ------------------------------------------------------------

;; which-key: show available keybindings in popup
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; ------------------------------------------------------------
;; 🗂 Org-mode Synergy
;; ------------------------------------------------------------

;; Indent Org content visually according to outline
(add-hook 'org-mode-hook #'org-indent-mode)

;; Soft-wrap lines in Org buffers
(add-hook 'org-mode-hook #'visual-line-mode)

;; Emoji font mapping for org-modern glyphs (Windows example)
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
;; For Linux/macOS, swap to "Noto Color Emoji" or "Apple Color Emoji"

(provide 'org-emacs)
;;; org-emacs.el ends here
