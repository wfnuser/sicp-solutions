(put 'drop 'complex
  (lambda (x) 
    ((if (eq? (imagine x) 0)
      (make-real (real x))
      (#f)))))

; (put 'drop 'real
;   (lambda (x) 
;     ((if (eq? (imagine x) 0)
;       (make-real (real x))
;       (#f)))))

(put 'drop 'rational
  (lambda (x) 
    ((if (eq? (denom x) 1)
      (make-integer (numer x))
      (#f)))))