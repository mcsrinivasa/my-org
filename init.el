;; EMACS startup configuration

;; Package system initialize
(package-initialize)

;; Default directory
(setq default-directory (expand-file-name "~/org/"))

;; Load path for custom modules
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Load customizations
(load "my-emacs-customizations")
(load "my-org-customizations")

;; Open capture.org on startup
(setq initial-buffer-choice (expand-file-name "~/org/capture.org"))

(provide 'init)
;; init.el ends here