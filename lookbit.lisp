;;Draw simple graph
(defun look-bits(n)
  (spark:spark (map 'list #'digit-char-p (write-to-string n :base 2))))
