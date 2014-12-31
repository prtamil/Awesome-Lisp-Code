(defun init ()
  (print 'initialise-stuff-here))

(defun draw ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)
  (print width )
  (print height)
  (gl:viewport 0 0 width height))

(defmacro continuable (&body body)
  "Helper macro since we use continue restarts a lot
   <remember to hit C in slime or pick the restart so errors don't kill the app>"
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
                        (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defun run-demo ()
  (sdl:with-init ()
    (sdl:window
     640 480 :opengl t
     :resizable t
     :opengl-attributes '((:sdl-gl-doublebuffer 1)
                          (:sdl-gl-alpha-size 0)
                          (:sdl-gl-depth-size 16)
                          (:sdl-gl-stencil-size 8)
                          (:sdl-gl-red-size 8)
                          (:sdl-gl-green-size 8)
                          (:sdl-gl-blue-size 8)))
    (init)
    (reshape 640 480)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height)
                           (reshape width height))
      (:idle ()
             (continuable (update-swank))
             (continuable (draw))))))
