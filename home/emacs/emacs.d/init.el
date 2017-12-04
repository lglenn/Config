
;;; MELPA

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;;; Prevent custom from pooping in ~/.emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Misc settings

;; turn on syntax hilighting
(global-font-lock-mode 1)

;;;; Modeline

;; Show time
(display-time)

;; Show file size 
(size-indication-mode)

;; Don't add duplicate kills to the kill ring
(setq kill-do-not-save-duplicates t)

;; Dedupe command history
(setq history-delete-duplicates t)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;;; Git
(require 'git)

;;; Org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@coop" . ?c)))
(setq org-tag-persistent-alist org-tag-alist)
(setq org-todo-keywords
      '((sequence "TODO (t!)" "PLAN (p!)" "NEXT-ACTION (n!)" "WAITING (w@/!)" "APPT (a!)" "DEFERRED (d!)" "|" "CANCELLED (c@)" "DONE (D!)")))

(setq org-log-done 'time)

;; Default location for org files
(setq org-directory "~/SpiderOak Hive/gtd/")
(setq org-gtd-inbox-file (concat org-directory "inbox.org"))
(setq org-gtd-tickler-file (concat org-directory "tickler.org"))

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
     (define-key global-map "\C-cc" 'org-capture)

(setq org-refile-targets (mapcar (lambda (e) (cons (concat org-directory (car e)) (cdr e)))
      '(
	("gtd.org" :maxlevel . 3)
      	("someday.org" :level . 1)
      	("tickler.org" :maxlevel . 2)
	)))
