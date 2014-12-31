(defun make-random-choice(lst)
  (let ((len (length lst)))
    (nth (random len) lst)))

(defun get-choice-summary(numbers lst)
  (let ((choice-lst (loop for i from 1 to numbers
		       collect (make-random-choice lst))))
    (loop for a in lst 
	  collect (list a (count a choice-lst)))))

(defparameter *c* '("lisp" "python" "js"))

(defparameter *res* (get-choice-summary 1000 *c*))

(format t "Choice Summary: ~%")
(loop for x in *res* do
     (format t " ~a   -> ~a~%" (first x) (second x)))
