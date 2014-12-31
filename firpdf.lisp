(defun myex(&optional (file #P"/home/tamil/tamil.pdf"))
  (pdf:with-document ()
    (pdf:with-page()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((matrix1 (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	    (pdf:set-font matrix1 36.0)
	    (pdf:move-text 100 800)
	    (pdf:draw-text "TAMILSELVAN")
	    (pdf:translate 230 500)
	    (loop repeat 150
		  for i = 0.75 then (* i 1.045)
		  do (pdf:in-text-mode
		       (pdf:set-font matrix1 i)
		       (pdf:set-rgb-fill (/ (random 255) 255.0)
					 (/ (random 255) 255.0)
					 (/ (random 255) 255.0))
		       (pdf:move-text (* i 3) 0)
		      (pdf:show-text "Tamil-Tharani"))
		     (pdf:rotate 13))))))
    (pdf:write-document file)))

(myex)
