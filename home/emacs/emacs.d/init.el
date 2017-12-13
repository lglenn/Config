
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

;;; My org-mode config
(require 'my-org-config)

;;; Use abbrev mode for text
(add-hook 'text-mode-hook (lambda () (abbrev-mode)))
