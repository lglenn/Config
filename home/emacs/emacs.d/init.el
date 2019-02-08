
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;;; MELPA

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
>                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; Prevent custom from pooping in this file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set location, because what good is a text editor if it doesn't know what
;; time sunrise and sunset are?
(setq calendar-latitude 40.7)
(setq calendar-longitude -74.0)
(setq calendar-location-name "New York, NY")

;; turn on syntax hilighting
(global-font-lock-mode 1)

;;;; Modeline

;; Use powerline
(require 'powerline)
(powerline-default-theme)

;; Show time
(display-time)

;; Show file size 
(size-indication-mode)

;; Don't add duplicate kills to the kill ring
(setq kill-do-not-save-duplicates t)

;; Dedupe command history
(setq history-delete-duplicates t)

;; Evil

;; Use it by default
(require 'evil)
  (evil-mode 1)

;; surround
(require 'evil-surround)
    (global-evil-surround-mode 1)

;; Ivy for completion
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;;; Git
(require 'git)

;;; My org-mode config
(require 'my-org-config)

;;; Use abbrev mode for text
(add-hook 'text-mode-hook (lambda () (abbrev-mode)))

;;; Toggle case
(require 'togglecase)
(global-set-key (kbd "C-~") 'togglecase-after)

;;; Calc
(global-set-key (kbd "M-+") 'calc)

;;; Calendar
(global-set-key (kbd "M-_") 'calendar)

;;; Put backup files in their own space so they don't pollute their "home" directories.
;; Stolen from http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html and slightly modified.

(defun my-backup-file-name (path)
  (let* (
	 (backup_root "~/.emacs.d/backup/")
	 (backup_file_path (replace-regexp-in-string "//" "/" (concat backup_root path "~") ))
	 (backup_dir (file-name-directory backup_file_path))
	)
    ;; Side effect: make the backup's directory if it does not exist.
    (make-directory backup_dir backup_dir)
    backup_file_path
    )
  )

(setq make-backup-file-name-function 'my-backup-file-name)

;; Favorites
(setq favorite-files '(("C-M-g" "~/jet/OKRs/2018/okrs.org")
		       ("C-c C-d" "~/jet/Drafts/drafts.org")))

(setq favorite-files-readonly '(("C-M-m" "~/jet/MeetingNotes/meetings.org")
				("M-*" "~/jet/People/Feedback/feedback.org")))

(defun favefile (keys filename open-fn)
  (lexical-let ((fn filename) (opener open-fn))
    (global-set-key (kbd keys) (let ((fn filename)) (lambda () (interactive) (funcall opener fn))))))

(defun favefiles (faves open-fn)
     (mapc
      (lambda (l)
	(let ((k (car l))
	      (f (nth 1 l)))
	  (favefile k f open-fn))) faves))

(favefiles favorite-files 'find-file)
(favefiles favorite-files-readonly 'find-file-read-only)
