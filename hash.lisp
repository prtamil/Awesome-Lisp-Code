(defun read-separator (str)
 (let
  ((*readtable* (copy-readtable *readtable* nil)))
  (set-macro-character #\, (lambda (stream char)
                            (declare (ignore char) (ignore stream))
                            'break))
  (read str nil)))

(set-macro-character #\{
 (lambda (str char)
  (declare (ignore char))
  (let
   ((*readtable* (copy-readtable *readtable* nil)))
   (set-macro-character #\} (lambda (stream char)
                             (declare (ignore char) (ignore stream))
                             'end))

   (let
    ((pairs (loop for key = (read str nil nil t)
                  for sep = (read str nil nil t)
                  for value = (read str nil nil t)
                  for end? = (read-separator str)
                  do (when (not (eql '=> sep)) (error "Expected =>, did not get"))
                  do (when (not (or (eql 'end end?) (eql 'break end?))) (error "Expected , or }"))
                  collect (list key value)
                  while (not (eql 'end end?))))
     (retn (gensym)))
    `(let
      ((,retn (make-hash-table :test #'equal)))
      ,@(mapcar
         (lambda (pair)
          `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
         pairs)
      ,retn)))))

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))


(defparameter h1 {"tam" => 1,
	           "yuv" => 2,
		   "tha" => 3,
		   "dhi" => 4})

 (loop for key being the hash-keys of h1 do
	       (format t "~a => ~a ~%" key (gethash key h1)))
	       
