

(defun load-packages()
  (push "c:/Lisp/packages/alexandria-master/" asdf:*central-registry*)
  (push "c:/Lisp/packages/closer-mop-master/" asdf:*central-registry*)
  (push "c:/Lisp/packages/cl-ppcre-master/" asdf:*central-registry*)
  (push "c:/Lisp/packages/optima-master/" asdf:*central-registry*)

  (asdf:operate 'asdf:load-op 'alexandria)
  (asdf:operate 'asdf:load-op 'closer-mop)
  (asdf:operate 'asdf:load-op 'cl-ppcre)
  (asdf:operate 'asdf:load-op 'optima))

