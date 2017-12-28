(defun togglecase (c)
  (let ((up (upcase c)))
    (if (= c up) (downcase c) up)))

(defun replace-char-after-with (c)
  (delete-char 1)
  (insert c)
  (backward-char))

(defun togglecase-after ()
  (interactive)
  (replace-char-after-with (togglecase (char-after))))

(provide 'togglecase)
