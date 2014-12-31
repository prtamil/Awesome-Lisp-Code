






(defclass bnt-node ()

  ((node-item :accessor item :initform nil :initarg :node-item)

   (left :accessor left :initform nil :initarg :left)

   (right :accessor right :initform nil :initarg :right)))






(defclass rbt-node (bnt-node)

  ((color :accessor color :initform :black :initarg :color)
   
   (parent :accessor parent :initform nil :initarg :parent)))




(defparameter *sentinel* (make-instance 'rbt-node))

(setf (parent *sentinel*) nil)

(setf (left *sentinel*) *sentinel*)

(setf (right *sentinel*) *sentinel*)



(defmethod init ((node rbt-node))

  (setf (left node) *sentinel*)

  (setf (right node) *sentinel*)

  node)




(defclass tree-item ()

  ((key :accessor key :initform nil :initarg :key)))


(defclass val-tree-item (tree-item)

  ((val :accessor val :initform nil :initarg :val)))




(defmethod item-less ((a tree-item) (b tree-item))

  (< (key a) (key b)))


(defmethod item-equal ((a tree-item) (b tree-item))

  (= (key a) (key b)))





(defmethod rbt-null ((x rbt-node))

  (eql *sentinel* x))






(defmethod rotate-left ((x rbt-node))

  (cond ((rbt-null x) nil)

        (t

         (let* ((y (right x))

                (b (left y)))

           (setf (right x) b)

           (setf (left y) x)

           (unless (rbt-null b)

             (setf (parent b) x))

           (unless (parent x)

             (setf (parent y) nil)

             (setf (parent x) y)

             (return-from rotate-left y))

           (setf (parent y) (parent x))

           (if (is-left-child x)

               (setf (left (parent x)) y)

             (setf (right (parent x)) y))

           (setf (parent x) y)

           nil))))



; der folgende code ist dual unter left-right x-y


(defmethod rotate-right ((y rbt-node))

  (cond ((rbt-null y) nil)

        (t

         (let* ((x (left y))

                (b (right x)))

           (setf (left y) b)

           (setf (right x) y)

           (unless (rbt-null b)

             (setf (parent b) y))

           (unless (parent y)

             (when (not (rbt-null x))
               
               (setf (parent x) nil))
               
             (setf (parent y) x)

             (return-from rotate-right x))
 
           (setf (parent x) (parent y))

           (if (is-right-child y)

               (setf (right (parent y)) x)

             (setf (left (parent y)) x))

           (setf (parent y) x)

           nil))))
   






;; back-values from insert item:

;; 1) new-root

;; 2) node-that-was-inserted-or-found

;; 3) item-already-been-in (t or nil)



(defmethod insert-item ((it tree-item) (root rbt-node))

(let ((p) (n) (new-root root))

  (setf p (loop  with q = root

                 with p = nil
 
                 finally (return p)

                 while (not (rbt-null q))

                 do

                 (if (item-equal it (item q))

                     (return-from insert-item (values root q t)))

                 (setf p q)

                 (if (item-less it (item q))

                     (setf q (left q))

                   (setf q (right q)))))

  (setf n (init (make-instance 'rbt-node :node-item it :color :red :parent p)))

  (if p

      (if (item-less it (item p))

          (setf (left p) n)

        (setf (right p) n))

    (setf new-root n))

  (setf new-root (fix-insert n new-root))

  (values new-root n nil)))





(defmethod left-to-grandparent ((node rbt-node))

  (eql (parent node) (left (parent (parent node)))))


(defmethod right-to-grandparent ((node rbt-node))

  (eql (parent node) (right (parent (parent node)))))


(defmethod uncle-over-right ((node rbt-node))

  (right (parent (parent node))))


(defmethod uncle-over-left ((node rbt-node))

  (left (parent (parent node))))



(defmethod is-right-child ((node rbt-node))

  (eql node (right (parent node))))


(defmethod is-left-child ((node rbt-node))

  (eql node (left (parent node))))


(defmethod blacken ((node rbt-node))

  (setf (color node) :black))


(defmethod redden ((node rbt-node))

  (setf (color node) :red))



(defmethod is-red ((node rbt-node))

  (eql (color node) :red))


(defmethod is-black ((node rbt-node))

  (eql (color node) :black))
    

(defmethod grand-parent ((node rbt-node))

  (parent (parent node)))






(defmethod fix-insert ((pivot rbt-node) (root rbt-node))

  (let ((new-root 

         (loop with y = nil

               with x = pivot

               with res = nil

               with new-root = root

               finally (return new-root)

               while (and (not (eql x new-root)) (is-red (parent x)))

               do

               (cond ((left-to-grandparent x)

                      (setf y (uncle-over-right x))

                      (cond ((is-red y)          

                             (blacken (parent x))

                             (blacken y)

                             (redden (grand-parent x))

                             (setf x (grand-parent x)))

                            (t
          

                             (when (is-right-child x)

                               (setf x (parent x))

                               (setf res (rotate-left x))

                               (when res

                                 (setf new-root res)))

                             (blacken (parent x))

                             (redden (grand-parent x))

                             (setf res (rotate-right (grand-parent x)))

                             (when res

                               (setf new-root res)))))

                     (t

                      (setf y (uncle-over-left x))

                      (cond ((is-red y)           

                             (blacken (parent x))

                             (blacken y)

                             (redden (grand-parent x))

                             (setf x (grand-parent x)))

                            (t
         
                             (when (is-left-child x)

                               (setf x (parent x))

                               (setf res (rotate-right x))

                               (when res

                                 (setf new-root res)))

                             (blacken (parent x))

                             (redden (grand-parent x))

                             (setf res (rotate-left (grand-parent x)))

                             (when res

                               (setf new-root res)))))))))

 

    (blacken new-root)

    new-root))




(defmethod find-item ((it tree-item) (root rbt-node))

)

(defmethod delete-item ((it tree-item) (root rbt-node))

)


(defmethod delete-node ((rbt-del-node rbt-node) (root rbt-node))
)


(defmethod fix-delete ((pivot rbt-node) (root rbt-node))

)



(defun itemize (n)

  (make-instance 'tree-item :key n))


(defun nil-tree ()

  *sentinel*)

       


(defparameter it1 (make-instance 'tree-item))
(defparameter it2 (make-instance 'tree-item))

(defparameter x (make-instance 'rbt-node))



(defmethod display-tree ((root bnt-node))

  (cond

   ((rbt-null root) nil)

   (t (list 

       (format nil "Color: ~A - Item ~A" (color root) (key (item root))) 

       (display-tree (left root)) 

       (display-tree (right root))))))





(defun test-tree (l)

  (let ((tree (nil-tree)))

    (loop for n in l

          finally (return tree)

          do

          (setf tree (insert-item (itemize n) tree)))))



(defun dt (l)

  (graph-app (display-tree (test-tree l))))

(defun dtt (tree)

  (graph-app (display-tree tree)))




(defun super-test (n k)

  (dotimes (i n)

    (let* ((len k)

           (lis nil))

      (if (zerop (mod i 50)) (format t "i = ~A~%" i))

      (dotimes (j len)
        
        (push (random 200) lis))

      (test-tree lis))))

            




























