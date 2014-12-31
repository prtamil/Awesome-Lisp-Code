(defun wtf()
  (let ((display (open-default-display)))
  (unwind-protect
      (let* ((window (create-window :parent (screen-root (display-default-screen display))
                                    :x 10
                                    :y 10
                                    :width 100
                                    :height 100
                                    :event-mask '(:exposure :key-press)))
             (gc (create-gcontext :drawable window)))
        (map-window window)
        (event-case (display :discard-p t)
          (exposure ()
            (draw-rectangle window gc 20 20 10 10 t)
            (draw-glyphs window gc 10 40 "Hello, World!")
            nil #| continue receiving events |#)
          (key-press ()
            t #| non-nil result signals event-case to exit |#))))
    (close-display display)))
