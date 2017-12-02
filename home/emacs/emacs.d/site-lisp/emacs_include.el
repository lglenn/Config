;;; TL;DR: Make "changes to .emacs" in this file.
;;;
;;; This gets included by .emacs; we do this because all kinds of stuff gets auto-appended to
;;; .emacs, so updates from this repo wipe those changes out. Ergo, the 'real' .emacs file is
;;; kept as change-free as possible; it contains just enough boilerplate to load and include
;;; this file.

;;; Misc settings

;; turn on syntax hilighting
(global-font-lock-mode 1)

;; Show time in the modeline
(display-time)

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

