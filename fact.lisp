(defun fact(x)
 (if (> x 0)
    (* x (fact (1- x)))
    1))

(defun fact-lr(x)
  (if (< x 1)
      1
      (* x (fact-lr (1- x)))))

;;i dont see any diff in running time and efficeieny
