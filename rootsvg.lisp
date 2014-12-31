
(defpackage meme (:use :cl :cl-svg))

(in-package meme)
;;; Based on http://billmill.org/static/viewji/viewji.html which is based in
;;; turn on http://nodebox.net/code/index.php/Superfolia_|_root_source_code
(defun radians (degrees)
  ;;; SVG doesn't like exponential notation in many places, so I avoid
  ;;; the DOUBLE-FLOAT PI.
  (/ (* degrees 3.141592653589793) 180.0))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun rgb (r g b)
  (flet ((normalize-to-byte (c)
           (truncate (* c 255))))
    (format nil "rgb(~{~A~^, ~})" (mapcar #'normalize-to-byte (list r g b)))))

(defun root (canvas x y &optional (angle 0) (depth 5) (alpha 1.0) (decay 0.005))
  (let ((w (* depth 6.0)))
    ;(format t "at DEPTH ~A~&" depth)
    (dotimes (i (* depth (random-range 10 20)))
      (let* ((v (/ depth 5.0))
             (color (rgb  (- 0.8 (* v 0.25))
                          0.8
                          (- 0.8 v))))
        (setf alpha (max 0.0 (- alpha (* i decay))))
        ;;; CL will start to use exponential notation when alpha gets
        ;;; very small, and SVG hates this.
        (when (> alpha 0.00001)
          (setf angle (+ angle (random-range -60 60)))
          (let ((dx (+ x (* (cos (radians angle)) w)))
                (dy (+ y (* (sin (radians angle)) w)))
                (group
                 (make-group canvas (:stroke color :fill color :opacity alpha
                                     :stroke-width (* depth 0.5)
                                     :fill-opacity (* alpha 0.6)
                                     :stroke-linecap "round"))))
            ;; dropshadow
            (draw group (:circle :cx (+ x depth 1) :cy (1- (+ y depth))
                         :r (/ w 3)) :stroke "none" :fill "black")
            ;; line segment to next position:
            (draw group (:line :x1 x :y1 y :x2 dx :y2 dy))
            ;; node
            (draw group (:circle :cx x :cy y :r (/ w 4)))
            ;; random branch
            (when (and (> depth 0) (> (random 1.0) 0.85))
              (root canvas x y (+ angle (random-range -60 60)) (1- depth) alpha))
            (setf x dx
                  y dy)))))
        (when (and (> depth 0) (> (random 1.0) 0.7))
          (root canvas x y angle (1- depth) alpha))))

(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700"))
       (rg (make-radial-gradient scene (:id :generate
                                        :cx "50%" :cy "50%" :r "50%")
             (stop :color "rgb(32, 38, 0)" :offset "0%")
             (stop :color "rgb(13, 15, 0)" :offset "100%"))))
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
               :fill (xlink-href rg))
  (root scene 350 350 (random 360) 7)
  (with-open-file (s #p"root.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))
