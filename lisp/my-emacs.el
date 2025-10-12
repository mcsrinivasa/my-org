;;; my-emacs.el --- General Emacs quality-of-life settings (no autosave/backup redirects)
;;; Commentary:
;; Clean UI, editing comfort, discoverability. No custom autosave/backup redirection
;; to avoid directory-permission and race warnings across OS/sync clients.

;;; Code:

;; ------------------------------------------------------------
;; 🖥 Interface Tweaks
;; ------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Smooth-ish scrolling, sane minibuffer
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 101
      ring-bell-function 'ignore
      inhibit-startup-screen t
      use-dialog-box nil)

;; Show line/column; keep modeline signal minimal
(setq column-number-mode t
      line-number-mode t)

;; ------------------------------------------------------------
;; 🔤 Editing Comfort
;; ------------------------------------------------------------
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 100)

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 200)

;; Keep scratch buffer in text-mode
(setq initial-major-mode 'text-mode)

;; ------------------------------------------------------------
;; 🔎 Discoverability
;; ------------------------------------------------------------
(when (require 'which-key nil t)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.4))

;; ------------------------------------------------------------
;; 🧠 Org UX niceties (non-controversial)
;; ------------------------------------------------------------
;; Visual indent & soft wrap in Org
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Emoji font mapping for glyphs (Windows example)
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
;; For Linux/macOS, swap to "Noto Color Emoji" or "Apple Color Emoji"

(provide 'my-emacs)
;;; my-emacs.el ends here
