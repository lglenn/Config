;;; TL;DR: Make "changes to .emacs" in this file.
;;;
;;; This gets included by .emacs; we do this because all kinds of stuff gets auto-appended to
;;; .emacs, so updates from this repo wipe those changes out. Ergo, the 'real' .emacs file is
;;; kept as change-free as possible; it contains just enough boilerplate to load and include
;;; this file.

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

;; Git
(require 'git)

;; Org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@coop" . ?c)))
(setq org-tag-persistent-alist org-tag-alist)
(setq org-todo-keywords
      '((sequence "TODO (t)" "DEFER (d)" "WAITING (w)" "|" "DONE (D)" "DELEGATED")))
(setq org-log-done 'time)

(setq home "/Users/larry.glenn/")

;; Default location for org files
(setq org-directory (concat home "SpiderOak Hive/Agenda/"))

;;; Mobile Org Config
;; See: http://orgmode.org/manual/MobileOrg.html#MobileOrg

;; Mobile files are exchanged here.
(setq org-mobile-directory (concat home "~/Dropbox (Personal)/Apps/MobileOrg"))

;; By default, files in org-agenda-files are included. Add other files by setting
;; org-mobile-files

(setq org-mobile-inbox-for-pull (concat org-directory "mobile-inbox.org"))
