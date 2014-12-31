;;;; Last modified: 2013-09-18 20:24:02 tkych
 
;; This script is in the public domain.
;; Latest version is available at https://gist.github.com/tkych/6509285
 
 
;;====================================================================
;; Radiation Monitor for Fukushima Daiichi Nuclear Power Station
;;====================================================================
;;
;; Usage:
;; ------
;;
;; * (load "fukushima-monitor.lisp")
;; => T
;;
;; * (fukushima-monitor:watch)
;; =>
;; 2013-09-11
;; Radiation Dose Rate(microSv/h) At the Main Gate
;; ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; T
;;
;; * (fukushima-monitor:watch :detail? t)
;; =>
;; 2013-09-11
;; Radiation Dose Rate(microSv/h) At the Main Gate
;;
;; 0 250 500
;; ˫------------------+-------------------˧
;; 16: 0:00 █▎
;; 16: 0:30 █▎
;; 16: 1:00 █▎
;; ~~~~~~~~~~~~~~~~ omitting from 1:30 to 22:00 ~~~~~~~~~~
;; 16: 22:30 █▎
;; 16: 23:00 █▎
;; 16: 23:30 █▎
;; T
 
;; Note:
;; -----
;;
;; * If the rate is too high, DON'T PANIC!
;;
;; 1. You should check the scale. try (fukushima-monitor:watch :detail? t).
;;
;; * 1 [Sv] = 1,000 [mSv] = 1,000,000 [microSv]
;;
;; * These rate are at the Main Gate of Fukushima Daiichi Nuclear Power Station.
;; Currently, that area is off-limits to public.
;;
;; * The air dose rate at the city hall of fukushima city (prefectural capital)
;; is 0.42 [microSv] (2013-09-11).
;; c.f. http://fukushima-radioactivity.jp/index.php
;; http://www.city.fukushima.fukushima.jp/soshiki/29/20130222.html
;; http://www.mext.go.jp/english/incident/1305092.htm
;;
;; * An average person over one normal day received 10 microSv as background dose.
;; c.f. http://en.wikipedia.org/wiki/Sievert
;; http://en.wikipedia.org/wiki/Background_radiation
;;
;; 2. Probably it is a bug, check the web page for real rate,
;; http://www.tepco.co.jp/en/nu/fukushima-np/f1/index-e.html
;;
;; * If nothing is printing, DON'T PANIC!
;;
;; 1. Probably it is a bug, check the web page for real rate,
;; http://www.tepco.co.jp/en/nu/fukushima-np/f1/index-e.html
;;
;; 2. Probably monitoring data url had been changed, check the url,
;; http://www.tepco.co.jp/en/nu/fukushima-np/f1/images/2013monitoring/f1-mp-tcnew-e.zip
 
 
;;--------------------------------------------------------------------
 
(in-package :cl-user)
 
(eval-when (:compile-toplevel :load-toplevel :execute)
(ql:quickload '(:cl-spark :drakma :flexi-streams :zip :cl-ppcre)))
 
(defpackage #:fukushima-monitor
(:use :cl)
(:export #:*monitoring-data-url*
#:watch))
 
(in-package #:fukushima-monitor)
 
 
;;--------------------------------------------------------------------
 
(defparameter *monitoring-data-url*
"http://www.tepco.co.jp/en/nu/fukushima-np/f1/images/2013monitoring/f1-mp-tcnew-e.zip")
 
(defun generate-tmp-file-name ()
(format nil "./TMP-fukushima-monitor-~D.zip" (get-universal-time)))
 
(defun >> (input-stream output-filespec
&key
(element-type (stream-element-type input-stream))
(if-does-not-exist :create)
(if-exists :append)
(external-format :default)
(buff-size 4096))
"Redirect from `input-stream' to `output-filespec' like output redirector \">>\" in shell."
(with-open-file (out output-filespec
:direction :output
:element-type element-type
:if-does-not-exist if-does-not-exist
:if-exists if-exists
:external-format external-format)
(loop
:with buf := (make-array buff-size :element-type element-type)
:for pos := (read-sequence buf input-stream)
:while (plusp pos)
:do (write-sequence buf out :end pos))))
 
(defun parse-zip (tmp-zip-file)
(zip:with-zipfile (z tmp-zip-file)
(zip:do-zipfile-entries (cvs-file-name entry z)
(when (search "tmp" cvs-file-name) ;Accoding to TEPCO, "tmp" means Temporary Monitoring Post!
(RETURN-FROM parse-zip
(values (flexi-streams:octets-to-string
(zip:zipfile-entry-contents entry))
cvs-file-name))))))
 
(defun extract-cvs-content (csv-string)
(let ((data nil)
(times nil)
(title nil))
(cl-ppcre:do-register-groups (time rate)
(".*?,(.*?),.*?,(.*?),.*?
" csv-string)
(when (stringp rate)
(let ((num (parse-integer rate :junk-allowed t)))
(if num
(progn
(push num data)
(push time times))
(setf title rate)))))
(values title data times)))
 
(defun extract-date (csv-file-name)
(cl-ppcre:regex-replace-all
"(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)\\d\\d"
(cl-ppcre:register-groups-bind (date-time)
("f1-tmp-(.*?)-e.csv" csv-file-name)
date-time)
"\\1-\\2-\\3"))
 
(defun watch (&key (detail? nil) (min 0) (max 500))
(let ((tmp-zip-file (generate-tmp-file-name)))
(unwind-protect
(progn
;; Fatch zip data
(with-open-stream
(in (handler-case
(drakma:http-request *monitoring-data-url* :want-stream t)
(error (c) (RETURN-FROM watch
(format *error-output*
"FAIL: zip-file fetching [~S]" (type-of c))))))
;; Generate zip file
(handler-case
(>> in tmp-zip-file :element-type '(unsigned-byte 8)
:if-exists :supersede)
(error (c) (RETURN-FROM watch
(format *error-output*
"FAIL: zip-file generating [~S]" (type-of c))))))
;; Parse zip file
(multiple-value-bind
(csv-string csv-file-name)
(handler-case
(parse-zip tmp-zip-file)
(error (c) (RETURN-FROM watch
(format *error-output*
"FAIL: zip-file parsing [~S]" (type-of c)))))
;; Parse csv string
(multiple-value-bind
(title data times)
(handler-case
(extract-cvs-content csv-string)
(error (c) (RETURN-FROM watch
(format *error-output*
"FAIL: cvs-file parsing [~S]" (type-of c)))))
;; Print result
(format t "~& ~A~% ~A~%~A"
(extract-date csv-file-name)
title
(if detail?
(cl-spark:vspark
(reverse data)
:labels (loop
:for d :in (reverse data)
:for tm :in (reverse times)
:collect (format nil "~A: ~5,,,@A" d tm))
:min min :max max)
(cl-spark:spark
(nreverse data) :min min :max max))))))
;; Cleanup
(when (probe-file tmp-zip-file)
(delete-file tmp-zip-file))))
T)
 
 
;;====================================================================
