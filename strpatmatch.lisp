(defparameter *str* "Tamilselvan")
(defparameter *pat* "ils")



(defun brute-force-patmatch(pat str)
  "Brute force pattern match"
  (let ((pl (1- (length pat)))
	(sl (1- (length str)))
	(lst nil))
    (loop for i from 0 to sl
       do(when (char= (aref pat 0)
		      (aref str i))
	   (loop for j from 0 to pl
	      do(when (char= (aref pat j)
			     (aref str (+ i j)))
		  (push  (aref str (+ i j)) lst)))))
    (if (= (length lst)
	   (length pat))
	(format t "Pattern found :~a ~%" lst)
	(format t "Pattern not found ~%"))))

;;; (brute-force-patmatch "sel" "tamilselvan")
;;; => pattern found : (l e s)
;;; (brute-force-patmatch "sev" "tamilselvan")
;;; => pattern not found

(defun rabin-karp(pat str)
  "Rabin karp algorithm"
  (let ((pl (1- (length pat)))
	(sl (1- (length str)))
	(pathash (cl-murmurhash:murmurhash pat))
	(lis nil))
    
    (loop for i from 0 to (- sl pl)
       do(let* ((substr (subseq str i (+ i  (length pat))))
		(substr-hsh (cl-murmurhash:murmurhash substr)))
	   (if (= pathash substr-hsh)
	       (progn
		 (format t "Found  string ~a ~%" (subseq str i (+ i (length pat))))
		 (setf lis t)
		 (return "Found")))))
    (if (equal lis t)
	"Found"
	"Not Found")))

;;; (rabin-karp "sel" "tamilselvan")
;;; =>  Found string : sel
;;; =>  "Found"
;;; (rabin-karp "sev" "tamilselvan")
;;; => "Not Found"




