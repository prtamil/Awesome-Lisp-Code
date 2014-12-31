;;Draw simple graph
;;very good one
(defun look-bits(n)
  (spark:spark (map 'list #'digit-char-p (write-to-string n :base 2))))

