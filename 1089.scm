(use-modules (ice-9 rdelim)
	     (ice-9 format))


(define (subsets seq n)
  ;;(format #t "n: ~a seq: ~a \n" n seq)
  (cond ((= n 0) (list '()))
	((null? seq) '())
	(else (append (map (lambda (left-sets)
			     (cons (car seq) left-sets))
			   (subsets (cdr seq) (- n 1)))
		      (subsets (cdr seq) n)))))

(do ((line (read-line) (read-line))
     (first #t #f))
    ((eof-object? line))
  (let ((seq (map string->number (string-tokenize line))))
    (if (not (= (car seq) 0))
	(begin (or first (newline))
	       (format #t "~{~{~d~^ ~}~%~}"
		       (subsets (cdr seq) 6))))))
