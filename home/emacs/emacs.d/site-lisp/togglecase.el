(defun togglecase (c)
  (let ((up (upcase c)))
    (if (= c up) (downcase c) up)))

(defun replace-char-after-with (c)
  (delete-char 1)
  (insert c)
  (backward-char))

(defun togglecase-after ()
  "Toggle the case of the character after point. Point does not move. Does nothing if the character has no case."
  (interactive)
  (replace-char-after-with (togglecase (char-after))))

(provide 'togglecase)
