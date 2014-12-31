(defun n-to-l(n)
  (loop for c across (format nil "~a" n)
     collect (- (char-code c) (char-code #\0))))
