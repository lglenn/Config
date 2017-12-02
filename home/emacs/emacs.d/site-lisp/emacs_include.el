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

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (define-key ruby-mode-map "C-m" 'newline-and-indent) ; Not sure if this line is 100% right but it works!
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))
