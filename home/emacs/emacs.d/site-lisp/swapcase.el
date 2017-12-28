(defun swapcase (c)
  (let ((up (upcase c)))
    (if (= c up) (downcase c) up)))

(defun replace-char-after-with (c)
  (delete-char 1)
  (insert c))

(defun swapcase-after ()
  (interactive)
  (replace-char-after-with (swapcase (char-after))))

(provide 'swapcase)
