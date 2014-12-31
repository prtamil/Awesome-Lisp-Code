#!/usr/local/bin/sbcl --script

(defun vector-map-transformer (stream subchar arg)
  (let* ((sexp (read stream t))
	 (fname (car sexp))
	 (args (cdr sexp)))
     (format t "(~a ~a ~a )~%" sexp fname args)))


(set-dispatch-macro-character #\# #\v
			      #'vector-map-transformer


(read-from-string "#v((lambda (x) (* x x)) '(1 2 3 4 5 6 7 8 9 999))")


