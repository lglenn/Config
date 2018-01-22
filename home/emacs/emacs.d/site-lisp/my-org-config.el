;;; Org-mode

;; So bookmarklets work
(require 'org-protocol)

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

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@coop" . ?c) ("@ssi" . ?s) ("@techpanel" . ?t)))
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
		  "DUPE(u!)"
		  "DONE(D!)")))

(setq org-log-done 'time)

;; Default location for org files
(setq org-directory "~/gtd/")
(setq org-gtd-inbox-file (concat org-directory "inbox.org"))
(setq org-gtd-tickler-file (concat org-directory "tickler.org"))
(setq org-gtd-tasks-file (concat org-directory "gtd.org"))
(setq org-gtd-mobile-inbox-file (concat org-directory "mobile-inbox.org"))

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
			org-gtd-inbox-file
			org-gtd-tickler-file
			org-gtd-tasks-file
			org-gtd-mobile-inbox-file))

;; Custom Agenda Views
(setq org-agenda-custom-commands
      '(("x" agenda)
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
			       "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
			      ("e" "Event [inbox]" entry
			       (file+headline org-gtd-inbox-file "Inbox")
			       "* APPT %^{Brief Description} %^g\nSCHEDULED: %^T\n%?\nAdded: %U")
                              ("T" "Tickler" entry
                               (file+headline org-gtd-tickler-file "Tickler")
                               "* %i%? \n %U")))

(setq org-default-notes-file org-gtd-inbox-file)

(setq org-refile-targets (mapcar (lambda (e) (cons (concat org-directory (car e)) (cdr e)))
      '(
	("gtd.org" :maxlevel . 3)
      	("someday.org" :level . 1)
      	("tickler.org" :maxlevel . 2)
	("inbox.org" :maxlevel . 2)
	)))

(provide 'my-org-config)
