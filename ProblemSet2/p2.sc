;main
(define (truth-table phi)
 geta phi (list) (getp phi (list)) (Len (getp phi (list)) 0))

(define (geta phi All Prop Len x)
 (if (= x 0)
     all
     (geta phi (cons(list (merge Prop (creb (- x 1) Len) (list)) (calc (rep phi (merge Prop (creb (- x 1) Len) (list))))) All) Prop Len (- x 1))))

(define (merge P B M)
 (if (null? P)
     M
     (merge (rest P) (rest B) (append merge (list (list (first P) (first B)))))))

(define (creb x y)
 (if (< (length (binary x (list)) 0) y)
     (false (- y (length (binary x (list)) 0)) (binary x (list)))
     (binary x (list))))

(define (calc phi)
 (if(boolean? phi)
    phi
    (case (first phi)
	((and) (reduce (and (map calc (rest phi)) #f)))
	((or) (reduce (or (map calc (rest phi)) #t)))
	((not) (not (first (map calc (rest phi))))))))

(define (binary x A)
 (if (= x 0)
     A
     (if (= (remainder x 2) 1)
	 (binary (floor (/ x 2)) (cons #t A))
	 (binary (floor (/ x 2)) (cons #f A)))))

(define (rep phi A)
 (if (null? A)
     phi
     (rep (repe phi(first (first A)) (second (first A))) (rest A))))

(define (repe phi x y)
 (if (null? phi)
     phi
     (if (= (first phi) x)
	 (cons y (repe (rest phi) x y))
	 (if (list? (first phi))
	     (cons (repe (first phi) x y) (repe (rest phi) x y))
	     (cons (first phi) (repe (rest phi) x y))))))

(define (length A x)
 (if (null? A)
     x
     (length (rest A) (+ x 1))))

(define (getp phi A)
 (if (null? phi)
     (dup A)
     (case (chks (first phi))
      ((0) (getp (rest phi) (getp (first phi) A)))
      ((1) (getp (rest phi) A))
      ((2) (getp (rest phi) (append A (list (first phi))))))))

(define (remd A)
 (if (null? A)
     A
     (if (= (chkp (rest A) (first A)) 1)
	 (remd (rest A))
	 (cons (rest A) (remd (rest A))))))

(define (chkd A x)
 (if (null? A)
     0
     (if (= (first A) x)
	 1
	 (chkd (rest A) x))))

(define (chks elem)
 (case elem
  ((#t) 1)
  ((#f) 1)
  ((and) 1)
  ((or) 1)
  ((not) 1)
  (else (if (symbol? elem)
	    2
	    0))))
