(use-modules (ice-9 format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
(define (once pred seq)
  (if (null? seq)
      #f
      (or (pred (car seq))
	  (once pred (cdr seq)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; points
(define (make-point x y) (cons x y))
(define (x-cord point) (car point))
(define (y-cord point) (cdr point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; segments
(define (make-segment p1 p2) 
  (if (< (x-cord p1) (x-cord p2))
      (cons p1 p2)
      (cons p2 p1)))

(define (segment-start seg) (car seg))
(define (segment-end seg) (cdr seg))

(define (point-of-segment? point segment)
  (let ((x0 (x-cord point))
	(y0 (y-cord point))
	(x1 (x-cord (segment-start segment)))
	(y1 (y-cord (segment-start segment)))
	(x2 (x-cord (segment-end segment)))
	(y2 (y-cord (segment-end segment))))
    (and (<= x0 (max x1 x2))
	 (>= x0 (min x1 x2))
	 (<= y0 (max y1 y2))
	 (>= y0 (min y1 y2))
	 (= (* (- x2 x1) (- y2 y0))
	    (* (- x2 x0) (- y2 y1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; half-line or ray
(define (make-ray point k) (cons point k))
(define (ray-start r) (car r))
(define (ray-slope r) (cdr r))

;; let k always be positive (k > 0)
(define (point-of-ray? point ray)
  (let ((k (ray-slope ray))
	(x0 (x-cord (ray-start ray)))
	(y0 (y-cord (ray-start ray)))
	(x  (x-cord point))
	(y  (y-cord point)))
    (and (>= x x0)
	 (>= y y0)
	 (= (* k (- x x0)) (- y y0)))))

;; y = k (x-x0) + y0
;;     y2 - y1
;; y = -------  * (x-x1) + y1 
;;     x2 - x1
;;
;;       (x2-x1)(kx0+y1-y0) - (y2-y1)x1
;; x' = -------------------------------
;;           k(x2-x1) - (y2 -y1)
;;
;; y' = k(x'-x0) + y0

(define (cross-point  ray seg)
  (let* ((k (ray-slope ray))
	 (x0 (x-cord (ray-start ray)))
	 (y0 (y-cord (ray-start ray)))
	 (x1 (x-cord (segment-start seg)))
	 (y1 (y-cord (segment-start seg)))
	 (x2 (x-cord (segment-end seg)))
	 (y2 (y-cord (segment-end seg)))
	 (dy (- y2 y1))
	 (dx (- x2 x1))
	 (dy0 (- y1 y0))
	 (n  (- (* dx (+ (* k x0)  dy0)) 
		(* dy x1)))
	 (d  (- (* k dx) dy)))
    (if (= d 0)
	'()
	(let* ((x (/ n d))
	       (y (+ (* k (- x x0)) y0))
	       (cross-point (make-point x y)))
	  ;;(format #t "Posibble cross ~a ~%" cross-point)
	  (if (and (point-of-ray? cross-point ray)
		   (point-of-segment? cross-point seg))
	      cross-point
	      '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	      
;; polygon

;;construtor
(define (make-polygon  points) 
  (cons 'polygon points))

(define (polygon-points p)
  (if (and (pair? p)
	   (eq? (car p) 'polygon))
      (cdr p)
      (error "Not a Polygon:" p)))

;;selectors     
(define (polygon-segments p)
  (if (and (pair? p)
	   (eq? (car p) 'polygon))
      (let ((points (polygon-points p)))
	(define (iter pts result)
	  (if (null? (cdr pts)) 
	      (cons (make-segment (car pts) (car points)) result)
	      (iter (cdr pts) 
		    (cons (make-segment (car pts) (cadr pts)) result))))
	(iter points '()))
      (error "Not a Polygon:" p)))

;;predicates
(define (point-of-polygon? point poly)
  (or (polygon-vertex? point poly) 
      (once (lambda (segment)
	      (point-of-segment? point segment))
	    (polygon-segments poly))))

(define (polygon-vertex? point poly)
  (member point (polygon-points poly)))



;; find all cross points of a ray and the polygon
(define (cross-points ray poly)
  (accumulate 
   (lambda (seg l)
     (let ((point (cross-point ray seg)))
       (if (and (not (null? point))
		(not (member point l)))
	   (cons point l)
	   l)))
   '()
   (polygon-segments poly)))

(define (contains-vertex-of-polygon? crosses polygon)
  (once (lambda (point)
	  (polygon-vertex? point polygon))
	crosses))

;; use the parity of the number of cross points between 
;; a chosen ray with end point of `point' and the polygon. 
;; start ray with slope k = 0, if one or more of the cross 
;; points belong to the vertices of the polygon, then try another 
;; ray by increasing k  with 1 until none of the cross points
;; is the vertex.

(define (ray-check point polygon)
  (let lp ((k 0))
    ;;(format #t "k : ~a ~%" k)
    (let* ((r (make-ray point k))
	   (crosses (cross-points r polygon)))
      ;;(format #t "crosses: ~a ~%" crosses) 
      (if (contains-vertex-of-polygon? crosses polygon)
	  (lp (+ 1 k)) 
	  (odd? (length crosses))))))


(define (within? point polygon)
  (or (point-of-polygon? point polygon)
      (ray-check point polygon)))

(define (judge-points polygon-points points)
  (let ((poly (make-polygon polygon-points)))
    (for-each (lambda (point)
		(if (within? point poly)
		    (display "Within\n")
		    (display "Outside\n")))
	      points)))

;; I/O

(define (read-cords)
  (make-point 
   (inexact->exact (read))
   (inexact->exact (read))))

(define (read-n-points n)
  (let lp ((n n) 
	   (result '()))
    (if (= n 0)
	(reverse result)
	(lp (- n 1) (cons (read-cords) result)))))


(do ((n (read) (read)) 
     (m (read) (read))
     (i 1 (+ 1 i)))
    ((= n 0))
  (or (= i 1) (newline))
  (format #t "Problem ~d:~%" i)
  (judge-points  
   (read-n-points n)
   (read-n-points m)))
