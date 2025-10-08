;;; my-org.el --- Org-mode cockpit configuration for reliability engineering
;;; Commentary:
;; Corrected filing to inbox.org (except journal), auto IDs, link helpers,
;; three-group task states, reliability captures, and fixed keybindings.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-capture)

(setq org-directory "~/org")
(setq config-directory "~/.emacs.d")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; ------------------------------------------------------------
;; Task state model (three groups)
;; ------------------------------------------------------------
(setq org-todo-keywords
      '((sequence
         "TODO(t)" "PROJ(p)"
         "NEXT(n)" "WAITING(w)" "HOLD(h)"
         "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "orange red" :weight bold))
        ("PROJ"      . (:foreground "deep sky blue" :weight bold))
        ("NEXT"      . (:foreground "gold" :weight bold))
        ("WAITING"   . (:foreground "orchid" :weight bold))
        ("HOLD"      . (:foreground "light steel blue" :weight bold))
        ("DONE"      . (:foreground "forest green" :weight bold))
        ("CANCELLED" . (:foreground "dim gray" :weight bold))))

;; Tags for cockpit slicing (optional)
(setq org-tag-alist
      '(("FMEA" . ?f) ("BUG" . ?b) ("LESSON" . ?l) ("DECISION" . ?d)
        ("High" . ?H) ("Medium" . ?M) ("Low" . ?L)))

;; -----------------------------
;; REFILE
;; -----------------------------
(setq org-refile-targets
      `((,(expand-file-name "inbox.org"   org-directory) :maxlevel . 3)
        (,(expand-file-name "work.org"    org-directory) :maxlevel . 3)
        (,(expand-file-name "life.org"    org-directory) :maxlevel . 3)
        (,(expand-file-name "journal.org" org-directory) :maxlevel . 3)
        (,(expand-file-name "dailyplan.org" org-directory) :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; -----------------------------
;; ORG-MODERN STYLING
;; -----------------------------
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-list '((?* . ?•)))
  (org-modern-checkbox '((?☐ . ?☑) (?❍ . ?✘)))
  (org-modern-tags t)
  (org-modern-timestamp t)
  (org-modern-block-fringe t)
  (org-modern-horizontal-rule t))

;; ------------------------------------------------------------
;; Capture templates (all to inbox.org except journal)
;; ------------------------------------------------------------
(setq org-capture-templates
      `(
        ;; Standard
        ("t" "Todo" entry
         (file+headline ,(concat org-directory "/inbox.org") "Tasks")
         (file ,(concat config-directory "/templates/my-org-todo.txt")))

        ("p" "Project" entry
         (file+headline ,(concat org-directory "/inbox.org") "Projects")
         (file ,(concat config-directory "/templates/my-org-project.txt")))

        ("n" "Note" entry
         (file+headline ,(concat org-directory "/inbox.org") "Notes")
         (file ,(concat config-directory "/templates/my-org-note.txt")))

        ("m" "Meeting" entry
         (file+headline ,(concat org-directory "/inbox.org") "Meetings")
         (file ,(concat config-directory "/templates/my-org-meeting.txt")))

        ("D" "Daily Plan" entry
         (file+headline ,(concat org-directory "/inbox.org") "Daily Plans")
         (file ,(concat config-directory "/templates/my-org-dailyplan.txt")))

        ;; Reliability specific
        ("f" "FMEA" entry
         (file+headline ,(concat org-directory "/inbox.org") "FMEA")
         (file ,(concat config-directory "/templates/my-org-fmea.txt")))

        ("b" "Bug" entry
         (file+headline ,(concat org-directory "/inbox.org") "Bugs")
         (file ,(concat config-directory "/templates/my-org-bug.txt")))

        ("l" "Lesson Learned" entry
         (file+headline ,(concat org-directory "/inbox.org") "Lessons")
         (file ,(concat config-directory "/templates/my-org-lesson.txt")))

        ("d" "Decision" entry
         (file+headline ,(concat org-directory "/inbox.org") "Decisions")
         (file ,(concat config-directory "/templates/my-org-decision.txt")))

        ;; Journal
        ("j" "Journal" entry
         (file+olp+datetree ,(concat org-directory "/journal.org"))
         (file ,(concat config-directory "/templates/my-org-journal.txt"))
         :tree-type week)
        ))

(global-set-key (kbd "<f6>") #'org-capture)

;; ------------------------------------------------------------
;; Auto-ID at capture and link helpers
;; ------------------------------------------------------------
(defun my/org-ensure-id-at (pos)
  "Ensure an :ID: exists at heading containing POS."
  (when (and pos (marker-buffer pos))
    (with-current-buffer (marker-buffer pos)
      (save-excursion
        (goto-char pos)
        (org-back-to-heading t)
        (org-id-get-create)))))

(defun my/org-capture-ensure-id ()
  "Ensure captured entry has an :ID: for linking."
  (when (boundp 'org-capture-last-stored)
    (my/org-ensure-id-at org-capture-last-stored)))

(add-hook 'org-capture-prepare-finalize #'my/org-capture-ensure-id)

(defun my/org-copy-id ()
  "Copy the :ID: of the current heading to the kill ring."
  (interactive)
  (org-back-to-heading t)
  (let ((id (org-id-get-create)))
    (kill-new id)
    (message "Copied ID: %s" id)))

(defun my/org-copy-id-link ()
  "Copy an id: link to the current heading with its title."
  (interactive)
  (org-back-to-heading t)
  (let* ((id (org-id-get-create))
         (title (nth 4 (org-heading-components)))
         (link (format "[[id:%s][%s]]" id (or title id))))
    (kill-new link)
    (message "Copied link: %s" link)))

(global-set-key (kbd "<f5>") #'my/org-copy-id-link)

;; ------------------------------------------------------------
;; Agenda setup and fixed keybindings
;; ------------------------------------------------------------
(global-set-key (kbd "C-c a") #'org-agenda)

(defun my/org-agenda-focus (files &optional command)
  "Temporarily set `org-agenda-files` to FILES and open agenda.
If COMMAND is non-nil, run that custom agenda command key."
  (let ((org-agenda-files
         (mapcar (lambda (f) (expand-file-name f org-directory)) files)))
    (if command
        (org-agenda nil command)
      (call-interactively #'org-agenda))))

;; F1: all
(global-set-key [f1] (lambda () (interactive)
                      (my/org-agenda-focus '("inbox.org" "work.org" "life.org"))))

;; F2: work cockpit
(global-set-key [f2] (lambda () (interactive)
                      (my/org-agenda-focus '("inbox.org" "work.org"))))

;; F3: life + inbox
(global-set-key [f3] (lambda () (interactive)
                      (my/org-agenda-focus '("inbox.org" "life.org"))))

(defun my/org-open (file)
  (interactive)
  (find-file (expand-file-name file org-directory)))

(global-set-key (kbd "C-c i") (lambda () (interactive) (my/org-open "inbox.org")))
(global-set-key (kbd "C-c w") (lambda () (interactive) (my/org-open "work.org")))
(global-set-key (kbd "C-c l") (lambda () (interactive) (my/org-open "life.org")))
(global-set-key (kbd "C-c d") (lambda () (interactive) (my/org-open "dailyplan.org")))

;; ------------------------------------------------------------
;; Custom agenda commands (property-based queries)
;; ------------------------------------------------------------
(setq org-agenda-custom-commands
      `(
        ;; Reliability cockpit
        ("W" "Work Cockpit"
         (,@(mapcan
             (lambda (task-type)
               (mapcar
                (lambda (sev)
                  `(tags ,(format "CATEGORY=\"%s\"&STATUS=\"TODO\"&SEVERITY=\"%s\"" task-type sev)
                         ((org-agenda-overriding-header ,(format "%s — %s" task-type sev))
                          (org-agenda-prefix-format '((tags . "%(org-entry-get nil \"Program\") "))))))
                '("High" "Medium" "Low")))
             '("FMEA" "BUG"))

          ;; LESSON archive (show all, usually DONE)
          (tags "CATEGORY=\"LESSON\""
                ((org-agenda-overriding-header "Lessons by Program")
                 (org-agenda-prefix-format '((tags . "%(org-entry-get nil \"Program\") ")))))

          ;; DECISION archive (show all, usually DONE)
          (tags "CATEGORY=\"DECISION\""
                ((org-agenda-overriding-header "Decisions by Program")
                 (org-agenda-prefix-format '((tags . "%(org-entry-get nil \"Program\") ")))))
          ))

        ;; Quick Lessons/Decisions
        ("L" "Lessons & Decisions"
         ((tags "CATEGORY=\"LESSON\""
                ((org-agenda-overriding-header "Lessons by Program")
                 (org-agenda-prefix-format '((tags . "%(org-entry-get nil \"Program\") "))))
          (tags "CATEGORY=\"DECISION\""
                ((org-agenda-overriding-header "Decisions by Program")
                 (org-agenda-prefix-format '((tags . "%(org-entry-get nil \"Program\") "))))))
         ))))

(provide 'my-org)
;;; my-org.el ends here
