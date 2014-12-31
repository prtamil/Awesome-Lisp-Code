(defun efficient-primelist(n)
  (let ((prime-selector-bit-array (make-array (1+ n)
                                              :element-type 'bit
                                              :initial-element 0)))
    (loop  for i from 2 to n
      when (zerop (bit prime-selector-bit-array i))
        collect i
                and do
                  (loop for j from (expt i 2) to n by i
                    do (setf (bit prime-selector-bit-array j) 1)))))


