(define (set-union A B)
 (uni (chk A '()) (chk B '()) '()))


(define (uni A B C)
 (if (null? A)
     (if (null? B)
	 C
	 (cons (first B) (append C (rest B))))
     (if (mem (first A) B)
	 (uni (rest A) B C)
	 (uni (rest A) B (cons (first A) C)))))

(define (chk A B)
 (if (null? A)
     B
     (if (mem (first A) B)
	 (chk (rest A) B)
	 (chk (rest A) (cons (first A) B)))))

(define (mem a B)
 (if (null? B)
     #f
     (if (= a (first B))
	 #t
	 (mem a (rest B)))))

(define (set-intersection A B)
 (inter (chk A '()) (chk b '()) '()))
 
(define (inter A B C)
 (if (null? A)
     C
     (if (mem (first A) B)
	 (inter (rest A) B (cons (first A) C))
	 (inter (rest A) B C))))

(define (set-minus A B)
 (minu (chk A '()) (chk B '()) '()))

(define (minu A B C)
 (if (null? A)
     C
     (if (mem (first A) B)
	 (minu (rest A) B C)
	 (minu (rest A) B (cons (first A) C)))))
