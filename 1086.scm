(use-modules (ice-9 rdelim))

(define (octal->decimal number-in-string)
  (define (oct-fractions->decimal number-string)
    (map (lambda (c) 
	   (- (char->integer c) (char->integer #\0)))
	 (cddr (string->list number-string))))

  (define (rational->list n d)
    (if (> n 0)
	(cons (quotient (* n 10) d)   
	      (rational->list (remainder (* n 10) d) d))
	'()))

  (define (iter result sequence)
    (if (null? sequence)
	result
	(iter (/ (+ result (car sequence)) 8) (cdr sequence))))

  (let* ((r (iter 0 (reverse (oct-fractions->decimal number-in-string))))
	 (l (rational->list (numerator r) (denominator r))) 
	 (result (apply string 
			 (map (lambda (x) 
				(integer->char (+ x (char->integer #\0)))) 
			   l))))
    (format #t "~a [8] = 0.~a [10]\n" number-in-string result)))
 			  

(do ((s (read-line) (read-line)))
    ((eof-object? s))
  (octal->decimal s))
