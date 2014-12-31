(defun rmdub(lst)
  (let ((prev (first lst)))
    (iter (for x in lst)
	  (if (not (equal x prev))
	      (progn
		(collect prev into prevs)
		(setf prev x)))
	  (finally (return (append prevs (list prev)))))))
	      


(defun rle(lst)
  (let ((prev (first lst))
	(idx  0)
	(cnt 0))
    (iter (for x in lst)
	  (incf idx)
	  (if (not (equal x prev))
	      (progn
		(collect (list cnt prev) into res)
		(setf prev x)
		(setf cnt 1))
	      (progn
		(incf cnt)))
	  (finally (return (append res (list (list cnt prev))))))))



(defun reverse-rle(lst)
  (let ((reslst nil))
    (iter (for (cnt chr) in lst)
	  (setf reslst (iter (for x from 1 to cnt)
			     (collect chr)))
	  (collect reslst))))

(defun rev-rle-lst-to-char-lst(lst)
  (mapcar #'(lambda (x)
	      (coerce x 'string)) lst))

(defun rev-rle-string(lst-as-rle)
  (let ((str "")
	(lst-of-string (rev-rle-lst-to-char-lst (reverse-rle lst-as-rle))))
    (iter (for x in lst-of-string)
	  (setf str (concatenate 'string str x))
	  (finally (return str)))))



(defun explode-str(str)
  (iter (for x in-vector str)
	(collect x)))

(defun implode-str(lst)
  (coerce lst 'string))
