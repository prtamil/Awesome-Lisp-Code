 (proclaim '(optimize (debug 3)))

(defun sq(x)
  (* x x))

(defun fact(x)
  (break)
  (if (= x 0)
      1
      (*  x (fact (1- x)))))