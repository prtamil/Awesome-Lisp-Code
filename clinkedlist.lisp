(defstruct node
  (data 0)
  (lnk nil))


(defun fillnode(uptox)
  (let ((newl nil)
	(tail nil))
    (iter (for x from 1 to uptox)
	  (if (equal newl nil)
	      (progn
		(setf newl (make-node :data x
				      :lnk nil))
		(setf tail newl))
	      (progn
		(setf (node-lnk tail) (make-node :data x :lnk nil))
		(setf tail (node-lnk tail))))) newl))
	    
       


(defun printlst(lst)
  (let ((l lst))
    (iter (while (not (equal l nil)))
	  (format t "~a => " (node-data l))
	  (setf l (node-lnk l))))
  (format t "~%"))


 
(defun copy-linkedlist(lst)
  (let ((curr lst)
	(newl nil)
	(tael nil))
    (iter (while (not (equal curr nil)))
	  (if (equal newl nil)
	      (progn
		(setf newl (make-node :data (node-data curr)
				      :lnk nil))
		(setf tael newl))
	      (progn
		(setf (node-lnk tael) (make-node :data 0 :lnk nil))
		(setf tael (node-lnk tael))
		(setf (node-data tael) (node-data curr))
		(setf (node-lnk tael) nil)))
	  (setf curr (node-lnk curr)))newl))

						 
 

(defun clone-lst(lst)
  (let ((res nil))
    (if (equal lst nil)
        nil
      (progn
        (setf res (make-node :data (node-data lst)))
        (setf (node-lnk res) (clone-lst (node-lnk lst)))
        res))))


(defun reverse-lst-cln(lst)
 (break)
  (let* ((rf  (clone-lst lst))
         (rl rf)
         (tmp nil)
         (prev nil))
    (iter (while (not (equal rl nil)))
          (setf tmp (node-lnk rl))
          (setf (node-lnk rl) prev)
          (setf prev rl)
          (setf rl tmp))prev))


(defun reverse-lst-cpy(lst)
  (let* ((rf  (copy-linkedlist lst))
         (rl rf)
         (tmp nil)
         (prev nil))
    (iter (while (not (equal rl nil)))
          (setf tmp (node-lnk rl))
          (setf (node-lnk rl) prev)
          (setf prev rl)
          (setf rl tmp))prev))


(defun tst(uptox)
  (let ((xx (fillnode uptox)))
    (printlst xx)
    (format t "~%")
    (printlst (reverse-lst-cln  xx))
    (format t "~%")
     (printlst xx))) 

(defun tst1(uptox)
  (let ((xx (fillnode uptox)))
    (printlst (reverse-lst-cln xx))))


(defun tst2(uptox)
  (let ((xx (fillnode uptox)))
    (printlst (reverse-lst-cpy xx))))
    
 
	
	
