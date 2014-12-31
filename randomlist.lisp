(defun make-random-list (length max)
  "Generate a non-repeating list of random numbers of length LENGTH.  The maximum value of the random numbers is MAX - 1."
  (declare (fixnum length max))
  (unless (plusp length)
    (error "LENGTH may not be negative."))
  (unless (plusp max)
    (error "Can't generate negative numbers."))
  (unless (<= length max)
    (error "Can't generate a non-repeating list when LENGTH > MAX"))
  (let ((unused (make-array max :element-type 'fixnum)))
    ;; create an array with each element set to its index
    (dotimes (idx max)
      (setf (aref unused idx) idx))
    ;; select a random index to pull from the array, then set the number
    ;; at the index to the last selectable element in the array.  Continue
    ;; until we have the requisite list length
    (loop for result-size from 0 upto (1- length)
       as num = (random (- max result-size))
       collect (aref unused num)
       do (setf (aref unused num) (aref unused (- max result-size 1))))))
