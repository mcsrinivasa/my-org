;; Org-Mode specific customizations

;; --------------------
;; Function Definitions
;; --------------------
(setq org-directory "~/org")

;; File handling
(defun my/org-open (file)
  (interactive)
  (find-file (expand-file-name file org-directory)))

;; Agenda Focus
(defun my-org-agenda-focus (files &optional command)
  "Temporarily set `org-agenda-files` to FILES and open agenda.
If COMMAND is non-nil, run that custom agenda command key."
  (let ((org-agenda-files
         (mapcar (lambda (f) (expand-file-name f org-directory)) files)))
    (if command
        (org-agenda nil command)
      (call-interactively #'org-agenda))))

;; Linking (Internal)
(defun my/copy-id-to-clipboard ()
  "Create an ID for the current heading if needed, then copy an Org link
to the kill ring. Works only in `org-mode` buffers."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an Org buffer"))

  (let* ((id (org-id-get-create))
         (heading (nth 4 (org-heading-components)))
         (link (format "[[id:%s][%s]]" id heading)))
    (kill-new link)
    (message "Copied link to kill ring: %s" link)))

;; Reset checkboxes
(defun org-reset-checkbox-state-maybe ()
 "Reset all checkboxes in an entry
 if the `RESET_CHECK_BOXES' property is set"
 (interactive "∗")
 (if (org-entry-get (point) "RESET_CHECK_BOXES")
 (org-reset-checkbox-state-subtree)))

;; -----------
;; Keybindings
;; -----------
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "<f6>") 'org-capture)

(global-set-key (kbd "C-c c") (lambda () (interactive) (my/org-open "capture.org")))
(global-set-key (kbd "C-c w") (lambda () (interactive) (my/org-open "work.org")))
(global-set-key (kbd "C-c p") (lambda () (interactive) (my/org-open "private.org")))
(global-set-key (kbd "C-c W") (lambda () (interactive) (my/org-open "dailyplan_work.org")))
(global-set-key (kbd "C-c P") (lambda () (interactive) (my/org-open "dailyplan_private.org")))
(global-set-key (kbd "C-c j") (lambda () (interactive) (my/org-open "journal.org")))
(global-set-key (kbd "C-c J") (lambda () (interactive) (my/org-open "journal_private.org")))
(global-set-key (kbd "C-c l") (lambda () (interactive) (my/org-open "log.org")))
(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)

;; Agenda focus
;; F1: all
(global-set-key [f1] (lambda () (interactive)
                      (my-org-agenda-focus '("work.org" "private.org"))))

;; F2: work cockpit
(global-set-key [f2] (lambda () (interactive)
                      (my-org-agenda-focus '("work.org"))))

;; F3: private
(global-set-key [f3] (lambda () (interactive)
                       (my-org-agenda-focus '("private.org"))))

;; -------------------
;; Task customizations
;; -------------------
(setq org-log-into-drawer 't) ;; log items into drawers 
(setq org-log-repeat 'time)   ;; log time when set to DONE in a repeating task
(setq org-log-reschedule 'time) ;;  log anytime a task is rescheduled

;; TODO sequence
(setq org-todo-keywords
      '((sequence
         "TODO(t!)" "NEXT(n!)" "WAITING(w@/!)" "DELEGATED(e@/!)"
         "SOMEDAY(s!/@)" "PROJ(p!)" "|"
         "DONE(d!/@)" "CANCELLED(c@/@)")
        (sequence
         "GOAL(g!)" "|" "ACHIEVED(a@)" "MISSED(m@)")))

;; -------------------
;; Tags customizations
;; -------------------
(setq org-tag-alist
      '(
        ;; --- Work as a group-tag umbrella ---
        (:startgrouptag)
        ("WORK")
        (:grouptags)
        ("STATUS")
        ("TYPE")
        ("DOMAIN")
        ("PROJECT")
        ("LOCATION")
        ("OWNER")
        (:endgrouptag)

        ;; --- Status (mutually exclusive) ---
        (:startgroup)
        ("STATUS")
        (:grouptags)
        ("ACTIVE"   . ?V)
        ("BLOCKER"  . ?x)
        ("RESOLVED" . ?R)
        ("CLOSED"   . ?C)
        (:endgroup)

        ;; --- Type (mutually exclusive) ---
        (:startgroup)
        ("TYPE")
        (:grouptags)
        ("CALL"     . ?A)
        ("BUG"      . ?b)
        ("FMEA"     . ?f)
        ("TASK"     . ?T)
        ("ERRAND"   . ?E)
        ("LESSON"   . ?S)
        ("DECISION" . ?N)
	("LEARN"    . ?z)
        (:endgroup)

        ;; --- Domain (non-exclusive group) ---
        (:startgrouptag)
        ("DOMAIN")
        (:grouptags)
        ("MECH"     . ?M)
        ("ELEC"     . ?e)
        ("FW"       . ?F)
        ("THERMAL"  . ?H)
        ("REL"      . ?r)
        (:endgrouptag)

        ;; --- Project (hierarchical, non-exclusive) ---
        ;; Ex: #+TAGS: [ PROJECT : EV_CHARGER GAGGIAPID ]

        ;; --- Location (non-exclusive) ---
        (:startgrouptag)
        ("LOCATION")
        (:grouptags)
        ("HOME"     . ?o)
        ("WORK"     . ?w)
        ("LAB"      . ?L)
        ("FIELD"    . ?y)
        (:endgrouptag)

        ;; --- Owner (mutually exclusive) ---
        (:startgroup)
        ("OWNER")
        (:grouptags)
        ("SELF"     . ?s)
        ("TEAM"     . ?g)
        ("VENDOR"   . ?n)
        ("CROSS"    . ?X)
        (:endgroup)
))

;; ---------------------
;; Agenda customizations
;; ---------------------
(setq org-deadline-warning-days 14) ;; Only show in agenda view, if deadline is within two weeks
(setq org-agenda-files '("~/org/capture.org" "~/org/work.org" "~/org/private.org"))

(setq org-habit-show-habits t
      org-habit-graph-column 50)

(custom-set-variables
 '(org-agenda-custom-commands
   '(("N" . "Agenda and all NEW Tasks")
     ("Nw" "Work Tasks"
      ((agenda ""
               ((org-agenda-span 'week)))
       (tags "NEW" nil))
      ((org-agenda-files
        '("~/org/work.org" "~/org/capture.org"))))
     ("Np" "Private Tasks"
      ((agenda ""
               ((org-agenda-span 'week)))
       (tags "NEW" nil))
      ((org-agenda-files
        '("~/org/private.org" "~/org/capture.org"))))
     ("T" . "Task Triage")
     ("w" . "Work Tasks")
     ("p" . "Private Tasks")
     ("Twp" "Projects"
      ((tags-todo "ACTIVE+TODO=\"PROJ\""
                  ((org-agenda-overriding-header "Active Projects")))
       (tags-todo "CLOSED+TODO=\"PROJ\""
                  ((org-agenda-overriding-header "Closed Projects"))))
      ((org-agenda-files
        '("~/org/work.org")))
      nil)
     ("Twt" "TODO Tasks"
      ((tags-todo "+PRIORITY=\"A\"-BUG-FMEA-LESSON-DECISION-CLOSED"
             ((org-agenda-overriding-header "High Priority")))
       (tags-todo "+PRIORITY=\"B\"-BUG-FMEA-LESSON-DECISION-CLOSED"
             ((org-agenda-overriding-header "Medium Priority")))
       (tags-todo "+PRIORITY=\"C\"-BUG-FMEA-LESSON-DECISION-CLOSED"
             ((org-agenda-overriding-header "Low Priority"))))
      ((org-agenda-files '("~/org/work.org")))
      nil)

     ("Twb" "Open/Resolved BUGS"
      ((tags-todo "+PRIORITY=\"A\"+BUG-FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "High Priority Bugs")))
       (tags-todo "+PRIORITY=\"B\"+BUG-FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "Medium Priority Bugs")))
       (tags-todo "+PRIORITY=\"C\"+BUG-FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "Low Priority Bugs"))))
      ((org-agenda-files '("~/org/work.org")))
      nil)

     ("Twf" "Open/Resolved FMEA Items"
      ((tags-todo "+PRIORITY=\"A\"-BUG+FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "High Priority FMEA")))
       (tags-todo "+PRIORITY=\"B\"-BUG+FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "Medium Priority FMEA")))
       (tags-todo "+PRIORITY=\"C\"-BUG+FMEA-LESSON-DECISION-TASK-ERRAND-CALL+(TODO=\"TODO\"|TODO=\"NEXT\")-CLOSED"
             ((org-agenda-overriding-header "Low Priority FMEA"))))
      ((org-agenda-files '("~/org/work.org")))
      nil)

     ("Twl" "Lessons Captured"
      ((tags "LESSON"
             ((org-agenda-overriding-header "Lessons"))))
      ((org-agenda-files
        '("~/org/work.org" "~/org/work.org_archive"))))
     
     ("Twd" "Decisions Made"
      ((tags "DECISION"
             ((org-agenda-overriding-header "Decisions"))))
      ((org-agenda-files
        '("~/org/work.org" "~/org/work.org_archive"))))
     
     ("TwB" "Closed Bugs"
      ((tags "BUG+CLOSED"
             ((org-agenda-overriding-header "Closed Bugs"))))
      ((org-agenda-files
        '("~/org/work.org" "~/org/work.org_archive"))))
     
     ("TwF" "Closed FMEA"
      ((tags "FMEA+CLOSED"
             ((org-agenda-overriding-header "Closed FMEA"))))
      ((org-agenda-files
        '("~/org/work.org" "~/org/work.org_archive"))))
     
     ("Tww" "Weekly Review"
      ((agenda ""
               ((org-agenda-span 'week)))
       (todo "DONE"
             ((org-agenda-overriding-header "Completed Tasks")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck Tasks"))))
      ((org-agenda-files
        '("~/org/work.org")))
      nil)
     
     ("Tpx" "Dashboard"
      ((tags "TASK"
             ((org-agenda-overriding-header "Tasks")))
       (tags "ERRAND"
             ((org-agenda-overriding-header "Errands")))
       (stuck "" nil))
      ((org-agenda-files
        '("~/org/private.org")))
      nil)
     
     ("Tpw" "Weekly Review"
      ((agenda ""
               ((org-agenda-span 'week)))
       (todo "DONE"
             ((org-agenda-overriding-header "Completed Tasks")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck Tasks"))))
      ((org-agenda-files
        '("~/org/private.org")))
      nil))))

(setq org-stuck-projects
      (list
       "+LEVEL=2+TODO<>\"\"/-DONE-CANCELLED"
       '("TODO" "NEXT" "WAITING" "DELEGATED" "SOMEDAY")
       (lambda () (my/org-project-not-stuck-p))
       ""))

;;---------------------
;; Refile Customization
;;---------------------
(custom-set-variables
 '(org-refile-targets '((org-agenda-files :maxlevel . 3))) ;; Refile up to max of 3 levels
 '(org-refile-use-outline-path 'file) ;; When refiling, show file name
)

;; ---------
;; Org Habit
;; ---------
(custom-set-variables
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m)))

;; -----------
;; Org Capture
;; -----------
(setq org-capture-templates
      '(	 
	 ("D" "Decision" entry
         (file+headline "~/org/capture.org" "Decision")
         (file "~/.emacs.d/templates/decision.txt")
         :empty-lines 1)
	 ("g" "Goal" entry
         (file+headline "~/org/capture.org" "Goals")
         (file "~/.emacs.d/templates/goal.txt")
         :empty-lines 1)
	 ("L" "Lesson" entry
         (file+headline "~/org/capture.org" "Lessons")
         (file "~/.emacs.d/templates/lesson.txt")
         :empty-lines 1)
	 ("l" "Log" entry
	  (file+olp+datetree "~/org/log.org")
	  "* %U - %^{Log}\n%?")
	 ("m" "Meeting" entry
         (file+headline "~/org/capture.org" "Meetings")
         (file "~/.emacs.d/templates/meeting.txt")
         :empty-lines 1)
	 ("s" "Someday" entry
         (file+headline "~/org/capture.org" "Tasks")
         (file "~/.emacs.d/templates/someday.txt")
         :empty-lines 1)
	 ("t" "Todo Task" entry
         (file+headline "~/org/capture.org" "Tasks")
         (file "~/.emacs.d/templates/todo.txt")
         :empty-lines 1)
	 ("w" "Weekly" entry
         (file+headline "~/org/capture.org" "Weekly Reviews")
         (file "~/.emacs.d/templates/weekly.txt")
         :empty-lines 1)
	 ("P" "Daily Plan")
	 ("Pp" "Private" plain
	  (file+olp+datetree "~/org/dailyplan_private.org")
	  (file "~/.emacs.d/templates/dailyplan.txt")
	  :empty-lines 1)
	 ("Pw" "Work" plain
	  (file+olp+datetree "~/org/dailyplan_work.org")
	  (file "~/.emacs.d/templates/dailyplan.txt")
	  :empty-lines 1)
	 ("J" "Journal")
	 ("Jp" "Private Journal" entry
	  (file+olp+datetree "~/org/journal_private.org")
	  "* %U - %^{Activity}\n%?")
	 ("Jw" "Work Journal" entry
	  (file+olp+datetree "~/org/journal.org")
	  "* %U - %^{Activity}\n%?")
	 ("R" "Reliability Items")
	 ("Rp" "New Program" entry
         (file+headline "~/org/capture.org" "Programs")
         (file "~/.emacs.d/templates/reliability_proj.txt")
         :empty-lines 1)
	 ("Rb" "Bug" entry
         (file+headline "~/org/capture.org" "Bugs")
         (file "~/.emacs.d/templates/reliability_bug.txt")
         :empty-lines 1)
	 ("Rf" "FMEA" entry
         (file+headline "~/org/capture.org" "FMEA")
         (file "~/.emacs.d/templates/reliability_fmea.txt")
         :empty-lines 1)
	 ))

;; ----------------------
;; Clocking Customization
;; ----------------------
(setq org-log-into-drawer "CLOCKING")

;; ---------------
;; Customize Babel
;; ---------------
(custom-set-variables
 '(org-babel-load-languages '((emacs-lisp . t) (dot . t) (python . t)))
 )

(provide 'my-org-customizations)
;; End of my-org-customizations.el
