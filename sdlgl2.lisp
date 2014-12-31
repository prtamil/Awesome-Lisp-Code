(defparameter *ii* 0.0)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 0.5 0 0)
    (gl:vertex -0.5 -0.5 0)
    (gl:color 0 0.5 0)
    (gl:vertex 0 0.5 0)
    (gl:color 0 0 0.5)
    (gl:vertex 0.5 (+ -0.5 0.0) 0)))
  ;; finish the frame
 
(defun main-loop ()
  (format t "Welcome to Tamilselvan GL Demo with SDL")
  (sdl:with-init ()
    (sdl:window 600 600 
		:flags sdl:sdl-opengl
		:icon-caption "TamilSDL"
		:opengl t
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
		:fps (make-instance 'sdl::fps-fixed :target-frame-rate 60))
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    ;; Init GL
    (gl:clear-color 0 0 0 0)
    ;;Init view pos
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
             #+(and sbcl (not sb-thread)) (restartable
                                           (sb-sys:serve-all-events 0))
             (restartable (draw))
	     (setf *ii* (+ *ii* 0.01))
	      (gl:flush)
	      (sdl:update-display)))))



;(mooain-loop)
