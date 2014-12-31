(defun ins-lst(data)
  (let ((nls (make-mynode :data data :lnk *mylst*)))
    (setf *mylst* nls)))

(defun prlst()
  (let ((nls *mylst*))
    (iter (while (not (equal nil  nls)))
      (format t "=>~a " (mynode-data nls))
      (setf nls (mynode-lnk nls)))))

  