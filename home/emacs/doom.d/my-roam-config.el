;;; Org-roam


(use-package! org-roam
   :init
   (setq org-roam-v2-ack t))

(after! org-roam

  (setq org-roam-directory (file-truename roam-directory))

  (setq org-roam-completion-everywhere t)

  (map! "C-c n l" #'org-roam-buffer-toggle
        "C-c n f" #'org-roam-node-find
        "C-c n g" #'org-roam-graph
        "C-c n i" #'org-roam-node-insert
        "C-c n c" #'org-roam-capture
        "C-c n j" #'org-roam-dailies-capture-today)
  `
  (add-hook! 'org-mode
    (map! "C-M-i" 'completion-at-point))

  (org-roam-setup)

  (require 'org-roam-protocol)

  ;; Set a nice side buffer view
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(provide 'my-roam-config)
