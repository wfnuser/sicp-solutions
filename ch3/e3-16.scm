(define (count-pairs x) 
       (if (not (pair? x))
              0
              (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1)))

(define x (list 1 2 3))
(count-pairs x)

(define y (list (list 1 2) 3))
(count-pairs y)



(define cycle (cons 1 (cons 2 (cons 3 '()))))
(set-cdr! (last-pair cycle) cycle)
(count-pairs cycle)


(define one (list 1))
(define three (cons one one))
(define seven (cons three three))
(count-pairs seven)