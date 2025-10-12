;;; my-mail.el --- Multi-account email (mu4e + mbsync + msmtp + OAuth2)
;;; Commentary: Gmail (OAuth2), Outlook (OAuth2), iCloud (app pwd), plus corp scaffolding
;;; Code:

(require 'mu4e)

;; Where mail is stored locally (by mbsync)
(setq mu4e-maildir (expand-file-name "~/.mail"))

;; Tell mu4e how to fetch
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300         ;; auto-refresh every 5 min
      mu4e-use-fancy-chars t
      mu4e-view-show-images t
      mu4e-attach-format " (%s)"
      mu4e-compose-format-flowed t)

;; Send via msmtp (multi-account aware)
(setq sendmail-program (executable-find "msmtp")
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Bookmarks (quick searches)
(setq mu4e-bookmarks
      `((:name "Unified Inbox"
               :query ,(mapconcat
                        #'identity
                        '(
                          "maildir:/gmail/INBOX"
                          "maildir:/icloud/INBOX"
                          "maildir:/outlook/INBOX"
                          ;; Uncomment corp inboxes when enabled:
                          ;; "maildir:/microsoft/INBOX"
                          ;; "maildir:/amazon/INBOX"
                          )
                        " OR ")
               :key ?i)
        (:name "Unread"
               :query "flag:unread AND NOT flag:trashed"
               :key ?u)
        (:name "Last 7 days"
               :query "date:7d..now"
               :key ?7)))

;; Common helpers for folder names
(defun m/maildirs (root)
  "Return standard Gmail-like folder mapping for ROOT."
  `(:inbox ,(format "/%s/INBOX" root)
    :sent  ,(format "/%s/[Gmail]/Sent Mail" root)
    :draft ,(format "/%s/[Gmail]/Drafts" root)
    :trash ,(format "/%s/[Gmail]/Trash" root)
    :archive ,(format "/%s/[Gmail]/All Mail" root)))

(defun m/std-maildirs (root)
  "Return standard non-Gmail folder mapping for ROOT."
  `(:inbox ,(format "/%s/INBOX" root)
    :sent  ,(format "/%s/Sent" root)
    :draft ,(format "/%s/Drafts" root)
    :trash ,(format "/%s/Trash" root)
    :archive ,(format "/%s/Archive" root)))

;; Contexts (choose account per message/folder)
(setq mu4e-contexts
      (list
       ;; Gmail (OAuth2)
       (let* ((f (m/maildirs "gmail")))
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg) (when msg
                                      (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . "manjevas@gmail.com")
                  (user-full-name    . "Manju Srinivasa")
                  (mu4e-drafts-folder . ,(plist-get f :draft))
                  (mu4e-sent-folder   . ,(plist-get f :sent))
                  (mu4e-trash-folder  . ,(plist-get f :trash))
                  (mu4e-refile-folder . ,(plist-get f :archive))
                  (message-sendmail-envelope-from . t)
                  (mail-envelope-from . "manjevas@gmail.com")
                  (smtpmail-smtp-user . "manjevas@gmail.com"))))

       ;; iCloud (App-specific password)
       (let* ((f (m/std-maildirs "icloud")))
         (make-mu4e-context
          :name "iCloud"
          :match-func (lambda (msg) (when msg
                                      (string-match-p "^/icloud" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . "manju.cs@icloud.com")
                  (user-full-name    . "Manju Srinivasa")
                  (mu4e-drafts-folder . ,(plist-get f :draft))
                  (mu4e-sent-folder   . ,(plist-get f :sent))
                  (mu4e-trash-folder  . ,(plist-get f :trash))
                  (mu4e-refile-folder . ,(plist-get f :archive))
                  (message-sendmail-envelope-from . t)
                  (mail-envelope-from . "manju.cs@icloud.com")
                  (smtpmail-smtp-user . "manju.cs@icloud.com"))))

       ;; Outlook/Hotmail (OAuth2 via Microsoft Identity)
       (let* ((f (m/std-maildirs "outlook")))
         (make-mu4e-context
          :name "Outlook"
          :match-func (lambda (msg) (when msg
                                      (string-match-p "^/outlook" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . "manjevas@outlook.com")
                  (user-full-name    . "Manju Srinivasa")
                  (mu4e-drafts-folder . ,(plist-get f :draft))
                  (mu4e-sent-folder   . ,(plist-get f :sent))
                  (mu4e-trash-folder  . ,(plist-get f :trash))
                  (mu4e-refile-folder . ,(plist-get f :archive))
                  (message-sendmail-envelope-from . t)
                  (mail-envelope-from . "manjevas@outlook.com")
                  (smtpmail-smtp-user . "manjevas@outlook.com"))))

       ;; ---- Scaffolding to enable later on work laptop ----

       ;; Microsoft Work (Office365 / Entra ID OAuth2)
       (let* ((f (m/std-maildirs "microsoft")))
         (make-mu4e-context
          :name "Microsoft-Work"
          :match-func (lambda (msg) (when msg
                                      (string-match-p "^/microsoft" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . "YOUR_ALIAS@microsoft.com")
                  (user-full-name    . "Manju Srinivasa")
                  (mu4e-drafts-folder . ,(plist-get f :draft))
                  (mu4e-sent-folder   . ,(plist-get f :sent))
                  (mu4e-trash-folder  . ,(plist-get f :trash))
                  (mu4e-refile-folder . ,(plist-get f :archive))
                  (message-sendmail-envelope-from . t)
                  (mail-envelope-from . "YOUR_ALIAS@microsoft.com")
                  (smtpmail-smtp-user . "YOUR_ALIAS@microsoft.com"))))

       ;; Amazon Work (Office365 / Entra ID OAuth2)
       (let* ((f (m/std-maildirs "amazon")))
         (make-mu4e-context
          :name "Amazon-Work"
          :match-func (lambda (msg) (when msg
                                      (string-match-p "^/amazon" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . "YOUR_ALIAS@amazon.com")
                  (user-full-name    . "Manju Srinivasa")
                  (mu4e-drafts-folder . ,(plist-get f :draft))
                  (mu4e-sent-folder   . ,(plist-get f :sent))
                  (mu4e-trash-folder  . ,(plist-get f :trash))
                  (mu4e-refile-folder . ,(plist-get f :archive))
                  (message-sendmail-envelope-from . t)
                  (mail-envelope-from . "YOUR_ALIAS@amazon.com")
                  (smtpmail-smtp-user . "YOUR_ALIAS@amazon.com"))))))

;; Start in a specific context? (nil = auto by folder)
(setq mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'ask-if-none)

;; Quality-of-life keys
(global-set-key (kbd "C-c m") #'mu4e)            ;; open mail
(global-set-key (kbd "C-c g") (lambda () (interactive) (mu4e~headers-jump-to-maildir "/gmail/INBOX")))
(global-set-key (kbd "C-c o") (lambda () (interactive) (mu4e~headers-jump-to-maildir "/outlook/INBOX")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (mu4e~headers-jump-to-maildir "/icloud/INBOX")))

(provide 'my-mail)
;;; my-mail.el ends here
