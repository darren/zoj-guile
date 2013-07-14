(use-modules (ice-9 format)
	     (ice-9 rdelim))

(define pi 3.141592653589793)

(define (make-point x y) (cons x y))
(define (x-cord point) (car point))
(define (y-cord point) (cdr point))

(define (square x) (* x x))

(define (distance p1 p2)
  (sqrt (+ (square (- (x-cord p1) (x-cord p2)))
	   (square (- (y-cord p1) (y-cord p2))))))

(define (triangle-area a b c)
  (let ((p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(define (circle-circumference p1 p2 p3)
  (let* ((a (distance p1 p2))
	 (b (distance p2 p3))
	 (c (distance p1 p3))
	 (s (triangle-area a b c))
	 (r (/ (* a b c) (* 4 s))))
    (* 2 pi r)))

(define (read-point)
  (cons (read) (read)))

(do ((p1 (read-point) (read-point))
     (p2 (read-point) (read-point))
     (p3 (read-point) (read-point)))
    ((eof-object? (peek-char)))
  (format #t 
	  "~,2f~%" 
	  (circle-circumference p1 p2 p3)))
	    
