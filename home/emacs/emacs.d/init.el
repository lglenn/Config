
;;; Make LaTeX available
(setenv "PATH" "$PATH:/Library/TeX/texbin/" t)

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

;;; Use environment variables from the shell
(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

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
  (evil-mode 0)

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

(all-the-icons-ivy-setup)

;;; Git
(require 'git)

;; Use Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; My org-mode config
(require 'my-org-config)
(require 'my-org-faces)

;;; org-roam config
(require 'my-roam-config)

;;; Use abbrev mode for text
(add-hook 'text-mode-hook (lambda () (abbrev-mode)))

;;; Toggle case
(require 'togglecase)
(global-set-key (kbd "C-~") 'togglecase-after)

;;; Calc
(global-set-key (kbd "M-+") 'calc)

;;; Calendar
(global-set-key (kbd "M-=") 'calendar)


;; Kill the "other" buffer opened by, e.g., help.
;; Stolen from https://www.emacswiki.org/emacs/KillingBuffers#h5o-5

(defun close-and-kill-next-pane ()
      "If there are multiple windows, then close the other pane and kill the buffer in it also."
      (interactive)
      (other-window 1)
      (kill-this-buffer)
      (if (not (one-window-p))
          (delete-window)))

(global-set-key (kbd "C-}") 'close-and-kill-next-pane)

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

(setq work-home-dir "~/walmart/")

;;; Stolen from http://www.mycpu.org/emacs-productivity-setup/

(require 'doom-themes)

(require 'indent-guide)
(indent-guide-global-mode)
(set-face-background 'indent-guide-face "dimgray")

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-molokai t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
; all-the-icons fonts must be installed!
; do do this: M-x all-the-icons-install-fonts
(doom-themes-neotree-config)  

(require 'doom-modeline)
(doom-modeline-mode 1)
