(defun len-after-remove-common-letters(name1 name2)
" Remove Common letters on given two names and calculate the 
  Total length of the removed letters"
  (let* ((tam name1)
	 (thr name2)
	 (a name1)
	 (b name2)
	 (f nil)
	 (g nil)
	 (l 0))
    (format t "Removing Common Letters~%")
    (iter (for x in-vector tam)
	  (setf thr (remove x thr)))
    (format t "--Name2 = ~a~%" thr) (setf f thr)
    (setf thr b)
    
    (iter (for y in-vector thr)
	  (setf tam (remove y tam)))
    (format t "--Name1 = ~a~%" tam) (setf g tam)
    (setf tam a)
    (setf l (reduce #'+ (mapcar #'length (list f g))))
    (format t "--Total Length = ~a~%" l)
    l))


(defun flames(name1 name2)
  "Flames Calulation"
  (let ((flstr "flames")
	(idx 0) 
	(sti 0)
	(len-removed (1- (len-after-remove-common-letters name1 name2))))
    (iter (while (> (length flstr) 1))
	  (iter (for i from 0 to len-removed)
		(setf idx (mod (+ i sti)  (length flstr)))
		(finally (progn
			   (setf flstr (remove (aref flstr idx) flstr))
			   (setf sti idx ))))
	  (format t "[~a] ~%" flstr))))
