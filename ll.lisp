(defstruct nd
  (data 0)
  (lnk nil))

(defun pplst(lst)
  (let ((ll lst))
    (loop while (not (equal ll nil))
	 do (progn
	      (format t "~a=>" (nd-data ll))
	      (setf ll (nd-lnk ll))))))

(defun lstval()
  (let* ((n (make-nd :data 0 :lnk nil))
	 (l n)
	 (x nil))
    (loop for i from 1 to 10
	 do(progn
	     (setf x (make-nd :data i :lnk nil))
	     (setf (nd-lnk n) x)
	     (setf n (nd-lnk n))))l))

;(defun pplst(lst)
;  (let ((ll lst))
;    (iter (while (not (equal ll nil)))
;	  (format t "~a=> " (nd-data ll))
;	  (setf ll (nd-lnk ll)))))

;(defun ll()
;	(let* ((n (make-nd :data 0 :lnk nil))
;	       (l n)
;	       (x nil))
;	  (iter (for i from 1 to 100)
;		(setf x (make-nd :data i :lnk nil))
;		(setf (nd-lnk n) x)
;		(setf n (nd-lnk n)))l))

(defparameter *llss* (lstval))
(pplst *llss*)