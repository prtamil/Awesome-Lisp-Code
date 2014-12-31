(defun mrg(ls1 ls2)
  (let ((l1 (1- (length ls1)))
	(l2 (1- (length ls2)))
	(i 0)
	(j 0)
	(res nil))
    (loop while (and (<= i l1) (<= j l2)) do
	 (if (< (nth i ls1) (nth j ls2))
	     (progn
	       (push (nth i ls1) res)
	       (incf i))
	     (progn
	       (push (nth j ls2) res)
	       (incf j))))
    (loop for x from i to l1 do
	 (push (nth x ls1) res))
    (loop for x from j to l2 do
	 (push (nth x ls2) res))
    
    (nreverse res)))


(defun seive-of-erotheses(n)
  (let ((bit-array (make-array (1+ n) :element-type 'bit :initial-element 0)))

    (loop for i from 2 to n
	  when (zerop (bit bit-array i))
	  collect i
	  and do 
	  (loop for j from (expt i 2) to n by i
		do (setf (bit bit-array j) 1)))))

