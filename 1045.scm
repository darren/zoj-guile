(use-modules (ice-9 format))

(define (f x)
  (define (iter i sum)
    (if (>= sum x) 
	(- i 2)
 	(iter (+ i 1) (+ sum (/ 1 i)))))
  (iter 2 0))

(do ((n (read) (read)))
    ((= n 0))
  (format #t "~d card(s)~%" (f n)))
	
