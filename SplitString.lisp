(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))
 
(defun write-with-periods (strings)
  (format t "~{~A~^.~}" strings))

;;Create Comma Seperated string from list of string

(defun lst-of-strings-to-csv(lstrings)
  (format nil "~{~A~^,~}" lstrings))

(defun csv-to-lst-of-strings(csv-string)
  (ppcre:split "\\," csv-string))
