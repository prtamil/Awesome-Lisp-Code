(defstruct node
  data
  (l nil)
  (r nil))

(defun bst-insert(root-node value)
  (iter 
    (initially 
     (when (null-node-p root-node)
       (setf (node-data root-node) value)
       (return root-node))
     (when (find-value root-node value)
       (return root-node)))
    (with cnode = root-node)
    (with pnode = nil)
    (with nnode = (make-node :data value))
    (until (null-node-p cnode))
    (let ((data (identity (node-data cnode))))
      (cond
	((< value data)
	 (setf pnode cnode)
	 (setf cnode (node-l cnode)))
	(t
	 (setf pnode cnode)
	 (setf cnode (node-r cnode)))))
    (finally
     (if (< value (identity (node-data pnode)))
	 (setf (node-l pnode) nnode)
	 (setf (node-r pnode) nnode))
     (return root-node))))

(defun find-value(root-node value)
  (iter
    (with cnode = root-node)
    (with pnode = nil)
    (until (null-node-p cnode))
    (let ((data (identity (node-data cnode))))
      (cond
	((= value data) (return (values cnode pnode)))
	((< value data) (setf pnode cnode)
	                (setf cnode (node-l cnode)))
	(t (setf pnode cnode)
	   (setf cnode (node-r cnode)))))
    (finally (return (values cnode pnode)))))


(defun null-node-p (n)
  (or (null n) (null (node-data n))))


(defun pg-bst-insert(root-node val)
  (if (null root-node)
      (make-node :data val)
      (let ((elt (node-data root-node)))
	(if (eql val elt)
	   root-node
	  (if (< val elt)
	      (make-node :data elt
			 :l (pg-bst-insert (node-l root-node) val)
			 :r (node-r root-node))
	      (make-node :data elt
		      :l (node-l root-node)
		      :r (pg-bst-insert (node-r root-node) val)))))))


;;;TEST


(defparameter *mytr1* (make-node))
(defparameter *mytr2* nil)

;Fill first param

(dolist (x '(1 4 3 2 9 8 7 6 5))
  (bst-insert *mytr1* x))

(dolist (y '(1 4 3 2 9 8 7 6 5))
  (setf *mytr2* (pg-bst-insert *mytr2* y)))
