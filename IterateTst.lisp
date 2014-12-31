(defun iter-test()
  (let ((i 0)
	(j 20))
    (iter (while t)
	  (format t "Beg ~%")
	  (setf i (1+ i))
	  (setf j (1- j))
	  (if (and (> i 19)
		   (< j 0))
	      (progn
		(format t "Leaving i=~a ,j=~a ~%" i j)
		(leave i))
	      (progn
		(format t "Continuing with i=~a,j=~a~%" i j)
		(next-iteration))))))




