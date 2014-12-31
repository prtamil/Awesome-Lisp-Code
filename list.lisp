(defstruct mynode 
  (data 0)
  (lnk nil))

(defvar *mylst* nil)

 (defun ins-lst(data)
  (let ((nls (make-mynode :data data :lnk *mylst*)))
    (setf *mylst* nls)))

(defun ins-back(data)
  (let ((nls *mylst*)
	(nnode (make-mynode :data data :lnk nil)))
	(iter (while (not (equal nil nls)))
	      (setf nls (mynode-lnk nls)))
	(setf (mynode-lnk nls) nnode)
	(format t "~a ~%" nls)))

(defun prlst()
  (let ((nls *mylst*))
    (iter (while (not (equal nil  nls)))
      (format t "=>~a " (mynode-data nls))
      (setf nls (mynode-lnk nls)))))

