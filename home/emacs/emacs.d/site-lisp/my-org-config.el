2;;; Org-mode

;; So bookmarklets work
(require 'org-protocol)

;; Show entries from the emacs diary in agenda by default
(setq org-agenda-include-diary t)

;; Use abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode)))

;; Wrap lines
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))

;; Pretty Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Various beauty tricks, including nice fonts and unicode bullets
;; Stolen from https://zzamboni.org/post/beautifying-org-mode-in-emacs

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-hide-emphasis-markers t)

(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Sans Pro" :height 1.1 :weight normal))))
 '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight medium :height 1.0 :width normal)))))

(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; Global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-tag-alist '(
                      ("@work" . ?w)
                      ("@home" . ?h)
                      ("@laptop" . ?l)
                      ("@bermuda" . ?b)
                      ))

(setq org-tag-persistent-alist org-tag-alist)
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "PLAN(p!)"
                  "WAITING(w@/!)"
                  "APPT(a!)"
                  "DEFERRED(d!)"
                  "DELEGATED(l@)"
                  "SCHEDULED(s!)"
                  "|"
                  "CANCELLED(c@)"
                  "EXPIRED(e!)"
                  "GARBAGE(g)"
                  "NOT ACTIONABLE(n)"
                  "DUPE(u!)"
                  "DONE(D!)")))

(setq org-log-done 'time)

(let
    ((org-directory "~/gtd/")
     (org-gtd-inbox-file (concat org-directory "inbox.org"))
     (org-gtd-tickler-file (concat org-directory "tickler.org"))
     (org-gtd-tasks-file (concat org-directory "gtd.org"))
     (meeting-notes-file "~/jet/MeetingNotes/meetings.org")
     (interviews-file "~/jet/People/Interviews/interviews.org")
     (feedback-file "~/jet/People/Feedback/feedback.org")
     (drafts-file "~/jet/Drafts/drafts.org")
     (glossary-file (concat org-directory "glossary.org"))
     (coach-file (concat org-directory "coach.org"))
     (refile-targets '((nil :level . 1)
                       ("gtd.org" :maxlevel . 3)
                       ("someday.org" :level . 1)
                       ("tickler.org" :maxlevel . 2)
                       ("inbox.org" :maxlevel . 2))))

  ;; Stick archive files in their own directory
  (setq org-archive-location "./archive/archive.org::* From %s")

  ;; Ideas from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (setq org-agenda-files (list
                          org-gtd-tickler-file
                          org-gtd-tasks-file))

  ;; Custom Agenda Views

  (defun skip-no-priority ()
    (and
     (not
      (member (org-entry-get nil "PRIORITY") '("A" "C")))
     (point-at-eol)))

  (setq org-agenda-custom-commands
	'(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
          ("p" "Agenda for items with non-default priority (A or C)" ((agenda "") (alltodo "")) ((org-agenda-skip-function 'skip-no-priority)))
          ("x" agenda)
          ("y" agenda*)
          ("w" todo "WAITING|DELEGATED")
          ("@" . "Special tag searches: @c: @coop @h: @home @s: @ssi @t: @techpanel @w: @work")
          ("@s" tags "+@ssi")
          ("@w" tags "+@work")
          ("@h" tags "+@home")
          ("@c" tags "+@coop")
          ("@t" tags "+@techpanel")))

  ;; Log todo state changes
  (setq org-log-into-drawer "LOGBOOK")

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline org-gtd-inbox-file "Inbox")
				 "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
				("e" "Express Todo (straight to Tasks -- write well!)" entry
				 (file+headline org-gtd-tasks-file "Tasks")
				 "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
				("p" "Todo [projects]" entry
				 (file+headline org-gtd-tasks-file "Projects")
				 "* %^{Brief Description} [/] %^g\n%?\nAdded: %U")
				("m" "Meeting" entry (file+headline meeting-notes-file "Meetings")
				 "* %^{Description}\n** Date: %^U\n** Attendees\n   - \n** Notes\n   - %?" :empty-lines 1)
				("i" "Interview" entry (file+headline interviews-file "Interviews")
				 "* %^{Candidate Name}\n** Date: %^U\n** Notes\n  - %?" :empty-lines 1)
				("f" "Feedback" entry (file+headline feedback-file "Feedback")
				 "* %^{Person}\n:PROPERTIES:\n:person: %\\1\n:END:\n** Date: %^U\n** Feedback\n*** Situation\n  %?\n*** Behavior\n*** Impact" :empty-lines 1)
				("d" "Draft" entry (file+headline drafts-file "Drafts")
				 "* %^{Subject}\n** Date: %^U\n** Notes\n%?" :empty-lines 1)
				("g" "Glossary" entry (file+headline glossary-file "Glossary")
				 "** %^{Term}\n:PROPERTIES:\n:term %\\1\n:END:\n %?" :empty-lines 1)
				("c" "Coaching Observation" entry (file+headline coach-file "Capture") "** %?\n")
				("T" "Tickler" entry
				 (file+headline org-gtd-tickler-file "Tickler")
				 "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")))

  (setq org-default-notes-file org-gtd-inbox-file)

  (setq org-refile-targets 
	(let ((prepend-directory-if-string
               (lambda (target)
		 (let ((file (car target)))
		   (if (stringp file)
		       (let ((path (concat org-directory file)))
			 (cons path (cdr target)))
                     target)))))
              (mapcar prepend-directory-if-string refile-targets))))

(provide 'my-org-config)
