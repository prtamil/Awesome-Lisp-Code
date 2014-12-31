;Seive of erotheses Alg
; One of the fastest way to find prime number
(defun seive-of-erotheses(n)
  (let ((bit-array (make-array (1+ n) :element-type 'bit :initial-element 0)))

    (loop for i from 2 to n
	  when (zerop (bit bit-array i))
	  collect i
	  and do 
	  (loop for j from (expt i 2) to n by i
		do (setf (bit bit-array j) 1)))))




;Added comment
(print (time (length (seive-of-erotheses 100000))))
