(defparameter *a1* #(1 2 3 4))
(defparameter *a2* #(5 6 7 8))
(defparameter *lst* '())

(defun mmerge(a1 a2)
	(let ((lst '())
	      (i 0)
	      (j 0))
	  (iter (while (or (< i (length a1))
			   (< j (length a2))))
		(if (or (>= j (length a2))
			(and (< i (length a1))
			     (< (aref a1 i)
				(aref a2 j))))
		    (progn
		      (push (aref a1 i) lst)
		      (setf i (1+ i)))
		    (progn
		      (push (aref a2 j) lst)
		      (setf j (1+ j)))))(reverse lst) ))

