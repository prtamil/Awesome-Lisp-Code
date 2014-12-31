(optima:match "aaabbbbddddddd121212121212"
 ((optima.ppcre:ppcre "^([A-z]+)([0-9]+)$" a n)
  (format t "~a~a~%"  n a)))

(optima:match "2013-12-31"
 ((optima.ppcre:ppcre "^(\\d{4})-(\\d{2})-(\\d{2})$" y m d)
  (format t "Year: ~a, Month: ~a, Day: ~a~%" y m d)))
