(defun seive(n)
  (let ((bitarray (make-array (1+ n) :element-type 'bit
			      :initial-element 0)))
    (iter (for i from 2 to n)
	  (when (zerop (bit bitarray i))
	    (collect i into primes)
	    (iter (for j from (expt i 2) to n by i)
		  (setf (bit bitarray j) 1)))
	  (finally (return primes)))))

;(seive 600851475143)

(defun primefac(num)
  (when (> num 1)
    (do ((x 2 (1+ x)))
	((zerop (mod num x))
	 (cons x (primefac (/ num x)))))))

	  
	  
