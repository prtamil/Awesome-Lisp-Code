
(defun rle(str)
  (let ((curr  (char str 0))
	(prev (char str 0))
	(res "")
	(cnt 1))
    (loop for x across (subseq str 1 (length str)) do
	 (progn
	   (setf curr x)
	   (if (char= curr prev)
	       (progn
		 (incf cnt)
		 (setf prev curr))
	       (progn
		 (setf res (concatenate 'string res (format nil "[~3,'0d]" cnt) (string prev)))
		 (setf prev curr)
		 (setf cnt 1)))))
    (setf res (concatenate 'string res (format nil "[~3,'0d]" cnt) (string prev)))
    res))



	
