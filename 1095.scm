(use-modules (ice-9 format)
	     (ice-9 syncase))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define stream-null? null?)
(define the-empty-stream '())
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
		   (stream-map proc (stream-cdr stream)))))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
	      stream))

(define (merge-streams s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream
		   s1car
		   (merge-streams (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream
		   s2car
		   (merge-streams s1 (stream-cdr s2))))
		 (else
		  (cons-stream
		   s1car
		   (merge-streams (stream-cdr s1)
				  (stream-cdr s2)))))))))

(define humble-numbers
  (cons-stream
   1
   (let ((s2 (scale-stream humble-numbers 2))
	 (s3 (scale-stream humble-numbers 3))
	 (s5 (scale-stream humble-numbers 5))
	 (s7 (scale-stream humble-numbers 7)))
     (merge-streams
      s7
      (merge-streams  
       s5
       (merge-streams 
	s3
	s2))))))

(do ((i (read) (read)))
	  ((= i 0))
	(format #t 
		"The ~d~a humble number is ~d.~%" 
		i
		(let ((r (remainder i 10))
		      (s (remainder (quotient i 10) 10)))
		  (if (= s 1)
		      "th"
		      (cond ((= r 1)  "st")
			    ((= r 2)  "nd")
			    ((= r 3)  "rd")
			    (else "th"))))
		(stream-ref humble-numbers (- i 0))))
