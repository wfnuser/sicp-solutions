(define (gcd a b) 
    (display a)
    (display b)
    (newline)
    (if (= b 0) a
        (abs (gcd b (modulo a b)))))

(define (make-rat n d) 
    (let ((g (gcd n d)))
    (display g)
    (if (< d 0) (cons (/ (* -1 n) g) (/ (* -1 d) g))
    (cons (/ n g) (/ d g)))))
    
(define (numer x) 
    (car x))

(define (denom x) 
    (cdr x))

(define (print-rat x) 
    (newline)
    (display (numer x)) 
    (display "/") 
    (display (denom x)))

(let ((x (make-rat 4 -18))) 
    (print-rat x)
    x)