(defun swap(arr i j)
  (let ((tmp (aref arr i)))
    (setf (aref arr i) (aref arr j))
    (setf (aref arr j)  tmp)))

(defun partition(arr lo hi)
  (let ((pivot (aref arr lo))
	(lastSmall lo)
	(j 0))
    (loop for k from (1+ lo) to hi
       do (progn
	    (setf j k)
	    (if (< (aref arr j) pivot)
		(progn
		  (setf lastSmall (1+ lastSmall))
		  (swap arr lastSmall j))
		)))
    (swap arr lastSmall j)lastSmall))
