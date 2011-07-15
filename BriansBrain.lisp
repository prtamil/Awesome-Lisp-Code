(ql:quickload "cl-opengl")
(ql:quickload "cl-glu")
(ql:quickload "cl-glut")


(defun make-brain(w h)
  (make-array (list h w) :initial-element :off))

(defun make-initialized-brain(w h)
  (let ((cells (make-brain w h))
        (mid (floor w 2)))
    (setf (aref cells 0 mid) :on)
    (setf (aref cells 0 (1+ mid)) :on)
    cells))
        
(defun rule(state neibhours)
  (case state
    (:on  :dying)
    (:dying  :off)
    (t (if (= 2 (count :on neibhours)) :on :off))))


(defun neibhours(cells x y)
  (let* ((mx (1- (array-dimension cells 1)))
         (my (1- (array-dimension cells 0)))
         (l (if (zerop x) mx (1- x)))
         (r (if (= x mx) 0 (1+ x)))
         (u (if (zerop y) my (1- y)))
         (d (if (= y my) 0 (1+ y))))
    (mapcar (lambda (x y)
              (aref cells y x))
            (list l x r l r l x r)
            (list u u u y y d d d))))
         
 (defun evolve(src)
   (let* ((w (array-dimension src 1))
          (h (array-dimension src 0))
          (dst (make-brain w h)))

     (loop for j below h
           do (loop for i below w
                    do (setf (aref dst j i)
                             (funcall 'rule (aref src j i) (neibhours src i j)))))
     dst))

(defun simulate(steps initial)
  (loop with brain = initial
        repeat steps
        do (Setf brain (funcall 'evolve brain))
           finally (return brain)))


(defun benchmark()
  (format *trace-output* "Benchmarking on ~A ~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version))

  (simulate 1000 (make-initialized-brain 16 16))

  (loop
    for (w h i) in '((32 32 32768)
                     (64 64 8192)
                     (128 128 2048)
                     (256 256 512)
                     (512 512 128)
                     (1024 1024 32)
                     (2048 2048 8)
                     (4096 4096 2))

    do (let ((initial (make-initialized-brain w h)))
         (format *trace-output* "***~Dx~D ~D iteration~:P ***~%" w h i)
         (time (simulate i initial))
         (finish-output *trace-output*)))
  (values))


;;Graphical

(defclass bb (glut:window)
  ((cells :accessor cells-of :initarg :cells))
  (:default-initargs
      :title "Brains Brain"
       :width 320 :height 200
       :pos-x 100 :pos-y 100
        :mode '(:double :rgb)))


(defmethod glut:display-window :before ((w bb))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:reshape ((w bb) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 250 -50 250 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

;  (let ((cells (cells-of w)))
 ;   (gl:ortho 0 (array-dimension cells 1) 0 (array-dimension cells  0) -1 1)))


(defun render-cell(x y cell)

  (flet ((draw-cell (x y)
           (gl:with-pushed-matrix
             (gl:translate x y 0)
             (gl:with-primitive :polygon
               (gl:vertex 0.1 0.1 0)
               (gl:vertex 0.9 0.1 0)
               (gl:vertex 0.9 0.9 0)
               (gl:vertex 0.1 0.9 0)))))

    (case cell
      (:on (gl:color 2.0 0.0 0.0)
         (draw-cell x y))
      (:dying (gl:color 1 1 1)
         (draw-cell x y)))))

(defmethod glut:display((w bb))
  (gl:clear :color-buffer)

  (let* ((cells (cells-of w))
         (w (array-dimension cells 1))
         (h (array-dimension cells 0)))

    (loop for j below h
          do (loop for i below w
                   do (render-cell i j (aref cells j i)))))

  (glut:swap-buffers))


(defmethod glut:idle ((w bb))
  (setf (cells-of w) (evolve (cells-of w)))
  (glut:post-redisplay))


(defun run-simulation ()
  (glut:display-window
    (make-instance 'bb :cells (make-initialized-brain 200 200))))
