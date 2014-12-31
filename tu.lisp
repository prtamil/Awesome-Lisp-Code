 (defun calc (x)
  (let ((tu 20))
    (loop for i from 20 to 40
	  do (let ((f 0))
	       (setf f (+ i f))
	       (format t "~a~%" f)))) )