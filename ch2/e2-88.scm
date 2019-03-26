(define (negate poly)
  (make-poly (variable poly) (map (lambda (each) (make-term (order each) (- coeff each))) (term-list poly)))
) 

(put 'sub '(polynomial polynomial) 
  (lambda (x y) (tag (add-poly x (contents (negate (tag y))))))) 