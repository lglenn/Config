;; Org-mode

;; Local variables
(load-file "~/.emacs.d/local-org-config.el")

;; So bookmarklets work
(require 'org-protocol)

;; Show entries from the emacs diary in agenda by default
(setq org-agenda-include-diary t)

;; Use abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode)))

;; Wrap lines
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))

;; Use org journal (https://github.com/bastibe/org-journal)
(require 'org-journal)

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
       (headline           `(:inherit default :weight normal :foreground "#dddddd", base-font-color)))

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

(setq org-tag-alist '(
                      ("@work" . ?w)
                      ("@home" . ?h)
                      ("@laptop" . ?l)
                      ))

(setq org-tag-persistent-alist org-tag-alist)
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "WAITING(w@/!)"
                  "DELEGATED(l@)"
                  "|"
                  "CANCELLED(c@)"
                  "EXPIRED(e!)"
                  "GARBAGE(g)"
                  "NOT ACTIONABLE(n)"
                  "DUPE(u!)"
                  "DONE(D!)")))

(setq org-log-done 'time)

;; Default location for org files

;; Stick archive files in their own directory
(setq org-archive-location "./archive/archive.org::* From %s")

;; Custom Agenda Views

(defun skip-no-priority ()
  (and
   (not
    (member (org-entry-get nil "PRIORITY") '("A" "C")))
   (point-at-eol)))

(setq org-agenda-custom-commands
      '(("n" . "Prefix for agendas with todos")
	("na" "Agenda and all TODOs" ((agenda "") (alltodo "")))
	("nA" "Agenda and all TODO by tag" ((agenda "") (tags-todo "@work") (tags-todo "@laptop") (tags-todo "@home") (tags-todo "-@work-@home-@laptop")))
	("nw" "Agenda and work-related TODOs" ((agenda "") (tags-todo "@work")))
	("np" "Personal agenda and TODOs"
	 ((agenda "") (tags-todo "@home") (tags-todo "@laptop"))
	 ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'notregexp ":@\\(home\\|laptop\\):"))))
        ("p" "Agenda for items with non-default priority (A or C)" ((agenda "") (alltodo "")) ((org-agenda-skip-function 'skip-no-priority)))
        ("x" agenda)
        ("y" agenda*)
        ("w" todo "WAITING|DELEGATED")
        ("@" . "Special tag searches: @h: @home @l: @laptop @w: @work")
        ("@h" tags "+@home")
        ("@l" tags "+@laptop")
        ("@w" tags "+@work")))

;; Log todo state changes
(setq org-log-into-drawer "LOGBOOK")

;; Ideas from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html and modified
(let* ((inbox "inbox.org")
       (tickler "tickler.org")
       (someday "someday.org")
       (tasks "gtd.org")
       (glossary "glossary.org")
       (coach "coach.org")
       (gtd-inbox-file (concat gtd-directory inbox))
       (gtd-tickler-file (concat gtd-directory tickler))
       (gtd-someday-file (concat gtd-directory someday))
       (gtd-tasks-file (concat gtd-directory tasks))
       (glossary-file (concat gtd-directory glossary))
       (coach-file (concat gtd-directory coach))
       (meeting-notes-file (concat work-directory "MeetingNotes/meetings.org"))
       (interviews-file (concat work-directory "People/Interviews/interviews.org"))
       (feedback-file (concat work-directory "People/Feedback/feedback.org"))
       (drafts-file (concat work-directory "Drafts/drafts.org"))
       (journal-file (concat personal-directory "Journal/journal.org"))
       (food-diary-file (concat personal-directory "Diet/food_diary.org")))
  (setq org-journal-dir (concat work-directory "Journal"))
  (setq org-journal-file-type 'weekly)
  (setq org-agenda-files (list gtd-tasks-file))
  (setq org-capture-templates (list
			       (list '"t" '"Todo [inbox]" 'entry
				     (list 'file+headline gtd-inbox-file '"Inbox")
				     '"* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
			       (list '"p" '"Todo [projects]" 'entry
				     (list 'file+headline gtd-tasks-file '"Projects")
				     '"* %^{Brief Description} [/] %^g\n%?\nAdded: %U")
			       (list '"P" '"Todo [serial projects]" 'entry
				     (list 'file+headline gtd-tasks-file '"Serial Projects")
				     '"* %^{Brief Description} [%] %^g\n** TODO %?\nAdded: %U")
			       (list '"m" '"Meeting" 'entry
				     (list 'file+headline meeting-notes-file '"Meetings")
				     '"* %^{Description}\n** Date: %^U\n** Agenda\n   - \n** Attendees\n   - \n** Notes\n   - %? \n** To-Do's\n"
				     ':empty-lines '1)
			       (list '"s" '"My Staff Meeting" 'entry
				     (list 'file+headline meeting-notes-file '"Meetings")
				     '"* Staff Meeting\n** Date: %^U\n** Agenda\n   - \n** Attendees\n   - \n** Notes\n   - %? \n** To-Do's\n"
				     ':empty-lines '1)
			       (list '"S" '"Staff Meeting as Attendee" 'entry
				     (list 'file+headline meeting-notes-file '"Meetings")
				     '"* %^{Description}\n** Date: %^U\n** Agenda\n*** My Update\n**** Operational Excellence\n**** Projects\n**** Programs\n**** Other\n** Attendees\n   - \n** Notes\n   - %? \n** To-Do's\n"
				     ':empty-lines '1)
			       (list '"o" '"One on One" 'entry
				     (list 'file+headline meeting-notes-file '"Meetings")
				     '"* 1:1: %^{Description}\n** Date: %^U\n** Agenda\n   - %? \n** Notes\n   -  \n** To-Do's\n"
				     ':empty-lines '1)
			       (list '"i" '"Interview" 'entry
				     (list 'file+headline interviews-file '"Interviews")
				     '"* %^{Candidate Name}\n** Date: %^U\n** Notes\n  - %?"
				     ':empty-lines '1)
			       (list '"f" '"Feedback" 'entry
				     (list 'file+headline feedback-file '"Feedback")
				     '"* %^{Person}\n:PROPERTIES:\n:person: %\\1\n:END:\n** Date: %^U\n** Feedback\n*** Situation\n  %?\n*** Behavior\n*** Impact"
				     ':empty-lines '1)
			       (list '"d" '"Draft" 'entry
				     (list 'file+headline drafts-file '"Drafts")
				     '"* %^{Subject}\n** Date: %^U\n** Notes\n%?"
				     ':empty-lines '1)
			       (list '"g" '"Glossary" 'entry
				     (list 'file+headline glossary-file '"Glossary")
				     '"** %^{Term}\n:PROPERTIES:\n:term: %\\1\n:END:\n %?"
				     ':empty-lines '1)
			       (list '"c" '"Coaching Observation" 'entry
				     (list 'file+headline coach-file '"Capture")
				     '"** %?\n")
			       (list '"w" '"Film and TV" 'entry
				     (list 'file+headline gtd-someday-file '"Movies")
				     '"** %?\n")
			       (list '"j" '"Journal Entry" 'entry
				     (list 'file+olp+datetree journal-file)
				     '"** %<%k:%M %p>\n%?\n" :tree-type 'week)
			       (list '"F" '"Food Diary (to select a date, invoke org-capture (C-c c) with a C-1 prefix)" 'entry
				     (list 'file+olp+datetree food-diary-file)
				     '"** %^{Meal?}\n   - %?\n")))
	
  (setq org-default-notes-file gtd-inbox-file)
  (setq org-refile-targets 
	(let ((refile-target-files (list (cons nil '(:level . 1))
					 (cons tasks '(:maxlevel . 3))
					 (cons someday '(:level . 1))
					 (cons tickler '(:maxlevel . 2))
					 (cons inbox '(:maxlevel . 2))))
	      (prepend-directory-if-string
	       (lambda (e)
		 (let ((file (car e))
		       (params (cdr e)))
		   (cond ((stringp file)
			  (cons (concat gtd-directory file) params))
			 (t e))))))
          (mapcar prepend-directory-if-string refile-target-files))))

(provide 'my-org-config)
