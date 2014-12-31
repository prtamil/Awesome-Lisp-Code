(defun backtrace-with-extra-info (&key (start 1) (end 20))
  (swank-backend::call-with-debugging-environment
   (lambda ()
     (loop for i from start to (length (swank-backend::compute-backtrace
                                        start end))
           do (ignore-errors (print-frame i))))))
(defun print-frame (i)
  (destructuring-bind (&key file position &allow-other-keys)
      (apply #'append
             (remove-if #'atom
                        (swank:frame-source-location i)))
    (let* ((frame (swank-backend::nth-frame i))
           (line-number (find-line-position file position frame)))
      (format t "~2@a: ~s~%~
                   ~:[~*~;~:[~2:*    At ~a (unknown line)~*~%~;~
                             ~2:*    At ~a:~a~%~]~]~
                   ~:[~*~;    Local variables:~%~{      ~a = ~s~%~}~]"
              i
              (sb-debug::frame-call (swank-backend::nth-frame i))
              file line-number
              (swank-backend::frame-locals i)
              (mapcan (lambda (x)
                        ;; Filter out local variables whose variables we
                        ;; don't know
                        (unless (eql (getf x :value) :<not-available>)
                          (list (getf x :name) (getf x :value))))
                      (swank-backend::frame-locals i))))))
(defun find-line-position (file char-offset frame)
  ;; It would be nice if SBCL stored line number information in
  ;; addition to form path information by default Since it doesn't
  ;; we need to use Swank to map the source path to a character
  ;; offset, and then map the character offset to a line number
  (ignore-errors
   (let* ((location (sb-di::frame-code-location frame))
          (debug-source (sb-di::code-location-debug-source location))
          (line (with-open-file (stream file)
                  (1+ (loop repeat char-offset
                            count (eql (read-char stream) #\Newline))))))
     (format nil "~:[~a (file modified)~;~a~]"
             (= (file-write-date file)
                (sb-di::debug-source-created debug-source))
             line))))