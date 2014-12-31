(defun isrt(a len)
 (let ((j 0)
       (k -1)
       (arr (copy-seq a)))
   (loop for i from 1 to len
	do (progn
	     (setf k (aref arr i))
	     (setf j (1- i))
	     (loop while (and (>= j 0)
			      (> k (aref arr j)))
		  do(progn
		      (setf (aref arr (1+ j))
			    (aref arr j))
		      (setf j (1- j))))
	     (setf (aref arr (1+ j))
		   k)
	     ))(reverse arr)))
		  
	    
