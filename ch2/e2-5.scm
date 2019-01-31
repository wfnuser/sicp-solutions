(define (product3 ans b)
    (if (= b 0) ans 
        (product3 (* ans 3) (- b 1))))

(define (product2 ans a)
    (if (= a 0) ans 
        (product2 (* ans 2) (- a 1))))

(define (cons a b)
   (product3 (product2 1 a) b))

(define (log2 ans e)
    (if ( = (modulo e 2) 0) (log2 (+ ans 1) (/ e 2))
        ans))

(define (log3 ans e)
    (if ( = (modulo e 3) 0) (log3 (+ ans 1) (/ e 3))
        ans))

(define (car e)
    (log2 0 e))

(define (cdr e)
    (log3 0 e))

(let ((x (cons 2 2))) 
    x)

(car 18)
(cdr 36)
