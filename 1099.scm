;; (use-modules (ice-9 format)
;; 	     (ice-9 rdelim)
;; 	     (ice-9 regex))

;; (for-each 
;;  (lambda (x)
;;    (cond ((string=? x "<br>") (newline))
;;  	 ((string=? x "<hr>") 
;; 	  (newline)
;; 	  (display "--------------------------------------------------------------------------------")
;; 	  (newline))
;;  	 (else (display x) (display " "))))
;;  (string-tokenize (read-string))) 

