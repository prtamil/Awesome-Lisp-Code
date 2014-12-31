(defparameter *a* (make-array '(4 4) :initial-contents '((1 2 3 4)
							 (11 12 13 14)
							 (21 22 23 24)
							 (31 32 33 34))))
(defun matrix-transpose(A)
  (let* ((m (array-dimension A 0))
	 (n (array-dimension A 1))
	 (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 below m do
	 (loop for j from 0 below n do
	      (setf (aref B j i) (aref A i j))))B))

(defun print-2d-matrix(A)
  (let ((m (array-dimension A 0))
	(n (array-dimension A 1)))
    (format t "  ~%")
    (loop for i from 0 below m do
	 (loop for j from 0 below n do
	      (format t " ~a " (aref A i j)))
	 (format t "~%"))))
  


(print-2d-matrix *a*)
(print-2d-matrix (matrix-transpose *a*))
