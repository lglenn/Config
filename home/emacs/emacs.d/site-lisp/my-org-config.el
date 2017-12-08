;;; Org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@coop" . ?c)))
(setq org-tag-persistent-alist org-tag-alist)
(setq org-todo-keywords
      '((sequence "TODO (t!)"
		  "PLAN (p!)"
		  "NEXT-ACTION (n!)"
		  "WAITING (w@/!)"
		  "APPT (a!)"
		  "DEFERRED (d!)"
		  "|"
		  "CANCELLED (c@)"
		  "DONE (D!)")))

(setq org-log-done 'time)

;; Default location for org files
(setq org-directory "~/SpiderOak Hive/gtd/")
(setq org-gtd-inbox-file (concat org-directory "inbox.org"))
(setq org-gtd-tickler-file (concat org-directory "tickler.org"))

;; Stick archive files in their own directory
(setq org-archive-location "./archive/archive.org::* From %s")

;; Mobile Org Config
;; See: http://orgmode.org/manual/MobileOrg.html#MobileOrg

;; Mobile files are exchanged here.
(setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrg")

;; By default, files in org-agenda-files are included. Add other files by setting
;; org-mobile-files

(setq org-mobile-inbox-for-pull (concat org-directory "mobile-inbox.org"))

;; Ideas from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files (mapcar (lambda (e) (concat org-directory e)) '("inbox.org"
								       "mobile-inbox.org"
								       "gtd.org"
								       "tickler.org")))

;; Log todo state changes
(setq org-log-into-drawer "LOGBOOK")

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                              (file+headline org-gtd-inbox-file "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline org-gtd-tickler-file "Tickler")
                               "* %i%? \n %U")))

(setq org-default-notes-file (concat org-directory "inbox.org"))

(setq org-refile-targets (mapcar (lambda (e) (cons (concat org-directory (car e)) (cdr e)))
      '(
	("gtd.org" :maxlevel . 3)
      	("someday.org" :level . 1)
      	("tickler.org" :maxlevel . 2)
	("inbox.org" :maxlevel . 2)
	)))

(provide 'my-org-config)
