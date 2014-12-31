(defun len-after-remove-common-letters(name1 name2)
" Remove Common letters on given two names and calculate the 
  Total length of the removed letters"
  (let* ((nm1 name1)
	 (nm2 name2)
	 (l 0))
    (format t "Removing Common Letters~%")
    (iter (for x in-vector name1)
	  (setf nm2 (remove x nm2 :count 1)))
    (format t "--Name2 = ~a~%" nm2) 
    
    (iter (for y in-vector name2)
	  (setf nm1 (remove y nm1 :count 1)))
    (format t "--Name1 = ~a~%" nm1) 

    (setf l (reduce #'+ (mapcar #'length (list nm1 nm2))))
    (format t "--Total Length = ~a~%" l)
    l))


(defun flames(name1 name2)
  "Flames Calulation"
  (let ((flstr "flames")
	(idx 0) 
	(startIdx 0)
	(len-removed (1- (len-after-remove-common-letters name1 name2))))
    (iter (while (> (length flstr) 1))
	  (iter (for i from 0 to len-removed)
		(setf idx (mod (+ i startIdx)  (length flstr)))
		(finally (progn
			   (setf flstr (remove (aref flstr idx) flstr))
			   (setf startIdx idx))))
	  (format t "[ ~a ] ~%" flstr))))


