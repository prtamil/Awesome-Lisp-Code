(defparameter *arr* #(57 48 79 65 15 33 52))

(defun ins-sort(arr)
  (let* ((a arr)
	 (beg 0)
	 (end (1- (length arr)))
	 (k 0)
	 (key -1))
    (loop for i from (1+ beg) to end
	 do(progn
	     (setf key (aref a i))
	     (setf k (1- i))
	     (loop while (and (>= k beg)
			      (> key (aref a k)))
		  do(progn
		      (setf (aref a (+ 1 k)) (aref a k))
		      (setf k (1- k))
		      ))
	     (setf (aref a (+ 1 k)) key)))a))

