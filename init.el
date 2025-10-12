;;; init.el --- Emacs startup configuration for reliability cockpit
;;; Commentary:
;; This init file sets up Emacs for Org-mode based reliability workflows.
;; It ensures the right load paths, core modules, and a clean startup
;; directly into inbox.org.

;;; Code:

;; ------------------------------------------------------------
;; Package system
;; ------------------------------------------------------------
(package-initialize)

;; ------------------------------------------------------------
;; Default directories
;; ------------------------------------------------------------
;; All file open/save (C-x C-f, dired, etc.) defaults to ~/org
(setq default-directory (expand-file-name "~/org/"))

;; ------------------------------------------------------------
;; Load paths for custom modules
;; ------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; ------------------------------------------------------------
;; Load core configuration modules
;; ------------------------------------------------------------
(load "my-emacs")   ;; general editor settings
(load "my-org")     ;; org-mode cockpit configuration
;;(load "my-mail")    ;; << NEW: email module we will create next!

;; ------------------------------------------------------------
;; Custom packages (installed via M-x package-install)
;; ------------------------------------------------------------
(custom-set-variables
 '(package-selected-packages
   '(org-modern
     org-tree-slide
     pdf-tools
     ;; mu4e                 ;; << Add mu4e for Gmail email
     async                ;; to support mbsync/mail sync
     )))

(custom-set-faces)

;; ------------------------------------------------------------
;; Startup behavior
;; ------------------------------------------------------------
;; Always open inbox.org on startup (skip splash)
(setq initial-buffer-choice (expand-file-name "~/org/inbox.org"))

(provide 'init)
;;; init.el ends here
