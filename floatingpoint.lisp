;; Comparison of floating point numbers
;; Liam Healy 2008-01-22 19:00:17EST floating-point.lisp
;; Time-stamp: <2008-01-26 13:22:04EST floating-point.lisp>
;; $Id: $

(defun decode-IEEE754 (float)
  "The significand (mantissa), exponent, and sign of the IEEE 754
   representation of a floating point number, given as integers.  It does
   not matter whether the actual representation follows IEEE.
   Values returned are significand, exponent, sign, bits in significand,
   bits in exponent."
  (flet ((esize (sigbits) (if (< sigbits 30) 8 11)))
    (if (zerop float)
	;; zero must be treated specially; the whole word is 0
	(let ((bits (1- (float-precision (float 1 float)))))
	  (values 0 0 0 bits (esize bits)))
	(let* ((bits (1- (float-digits float)))
	       (expbits (esize bits)))
	  (multiple-value-bind (significand exponent sign)
	      (integer-decode-float float)
	    (if (< (float-precision float) (float-digits float))
		;; subnormalized number
		(values
		 (ash significand	; shift right
		      (- (float-precision float) (float-digits float)))
		 0	     ; exponent is 0 for subnormalized numbers
		 sign
		 bits
		 expbits)
		;; normalized number
		(values
		 (mask-field (byte bits 0) significand)	; leading "1" implicit
		 (+ (1- (expt 2 (1- expbits))) bits exponent) ; bias exponent
		 sign
		 bits
		 expbits)))))))

(defun format-IEEE754-bits (float &optional (stream t))
  "Format as binary each of the three pieces that make
   the IEEE 754 floating point representation for a float."
  (multiple-value-bind (mant exp sign sigbits expbits)
      (decode-IEEE754 float)
    (format stream "~1d ~v,'0b ~v,'0b"
	    (if (minusp sign) 1 0) expbits exp sigbits mant)))

(defun float-as-integer (float)
  "An integer corresponding to the float which satisfies three properties:
   1) For two floats (< a b), then
      (< (float-as-integer a) (float-as-integer b)).
   2) If two floats (< a b) are adjacent, then
      (= (1+ (float-as-integer a)) (float-as-integer b)).
   3) (zerop (float-as-integer 0.0))
   The absolute value of the integer is the integer of the
   IEEE754 representation without the sign bit, and the
   sign of the integer agrees with the sign of the float."
  (multiple-value-bind (mant exp sign sigbits expbits)
      (decode-ieee754 float)
    (declare (ignore expbits))
    (* sign (+ (ash exp sigbits) mant))))

;; (float-as-integer most-negative-single-float)
;; -2139095039
;; (float-as-integer least-negative-single-float)
;; -1
;; (float-as-integer 0.0f0)
;; 0
;; (float-as-integer least-positive-single-float)
;; 1
;; (float-as-integer (- 1.0f0 single-float-negative-epsilon))
;; 1065353215
;; (float-as-integer 1.0f0)
;; 1065353216
;; (float-as-integer (+ 1.0f0 single-float-epsilon))
;; 1065353217
;; (float-as-integer most-positive-single-float)
;; 2139095039

(defun integer-as-float (integer float-type)
  "Construct the floating point number from its integer representation.
   Also return the number in rational form."
  (let ((expbits (if (eq float-type 'double-float) 11 8))
	(sigbits (if (eq float-type 'double-float) 52 23)))
    (let* ((pinteger (abs integer))
	   (bexponent (ldb (byte expbits sigbits) pinteger))
	   (significand
	    (ldb (byte sigbits 0) pinteger))
	   (normalizedp (plusp bexponent)) ; normalized number
	   (frac (if normalizedp 1 0)))
      (dotimes (pos sigbits)
	;; compute the fractional part
	(incf frac
	      (if (logbitp (- sigbits pos 1) significand)
		  (expt 2 (- (+ (if normalizedp 1 0) pos)))
		  0)))
      (let ((result
	     (* (signum integer)	; preserve the sign
		(* (expt 2 (- bexponent (1- (expt 2 (1- expbits)))))
		   frac))))
	(values (coerce result float-type) result)))))


;; (integer-as-float -2139095039 'single-float)
;; -3.4028235e38
;; -340282346638528859811704183484516925440
;; (integer-as-float -1 'single-float)
;; -1.4012985e-45
;; -1/713623846352979940529142984724747568191373312
;; (integer-as-float 0 'single-float)
;; 0.0
;; 0
;; (integer-as-float 1 'single-float)
;; 1.4012985e-45
;; 1/713623846352979940529142984724747568191373312
;; (integer-as-float 1065353215 'single-float)
;; 0.99999994
;; 16777215/16777216
;; (integer-as-float 1065353216 'single-float)
;; 1.0
;; 1
;; (integer-as-float 1065353217 'single-float)
;; 1.0000001
;; 8388609/8388608
;; (integer-as-float 2139095039 'single-float)
;; 3.4028235e38
;; 340282346638528859811704183484516925440