;;; Org-mode

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

;; Global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-tag-alist '(
		      ("@work" . ?w)
		      ("@home" . ?h)
		      ("@laptop" . ?l)
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

;; Default location for org files
(setq org-directory "~/gtd/")
(setq org-ancillary-directory (concat org-directory "ancillary/"))
(setq org-gtd-inbox-file (concat org-ancillary-directory "inbox.org"))
(setq org-gtd-tickler-file (concat org-ancillary-directory "tickler.org"))
(setq org-gtd-tasks-file (concat org-directory "gtd.org"))
(setq org-gtd-mobile-inbox-file (concat org-ancillary-directory "mobile-inbox.org"))
(setq meeting-notes-file "~/jet/MeetingNotes/meetings.org")
(setq interviews-file "~/jet/People/Interviews/interviews.org")
(setq feedback-file "~/jet/People/Feedback/feedback.org")
(setq drafts-file "~/jet/Drafts/drafts.org")
(setq glossary-file "~/Dropbox/org/glossary.org")

;; Stick archive files in their own directory
(setq org-archive-location "./archive/archive.org::* From %s")

;; Mobile Org Config
;; See: http://orgmode.org/manual/MobileOrg.html#MobileOrg

;; Mobile files are exchanged here.
(setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrg")

;; By default, files in org-agenda-files are included. Add other files by setting
;; org-mobile-files

(setq org-mobile-inbox-for-pull org-gtd-mobile-inbox-file)

;; Ideas from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files (list
			org-gtd-tickler-file
			org-gtd-tasks-file
			org-gtd-mobile-inbox-file))

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
			      ("T" "Tickler" entry
                               (file+headline org-gtd-tickler-file "Tickler")
                               "* %i%? \n %U")))

(setq org-default-notes-file org-gtd-inbox-file)

(setq org-refile-targets (mapcar (lambda (e) (cons (concat org-directory (car e)) (cdr e)))
      '(
	("gtd.org" :maxlevel . 3)
      	("ancillary/someday.org" :level . 1)
      	("ancillary/tickler.org" :maxlevel . 2)
	("ancillary/inbox.org" :maxlevel . 2)
	)))

(provide 'my-org-config)
