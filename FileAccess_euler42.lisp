(defun string-sum (str)
  (loop for c across str sum (1+ (- (char-code c) (char-code #\A)))))

(defun triangle-number-p (n)
  (let* ((i (1+ (* 8 n)))
         (j (isqrt i)))
    (and (= i (* j j)) (oddp j) (<= 3 j))))

(defun euler-42 ()
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (with-open-file (s "c:/emacs/words.txt")
      (loop for word = (read s nil)
            while word
            when (triangle-number-p (string-sum word))
              count word))))



(defun parse-csv-file (file)
  (with-open-file (f file :direction :input)
    (loop
       for line = (read-line f nil)
       while line
       collect (cl-ppcre:split "," line))))

