(define (accumulate op initial sequence) 
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (
        cons (p x) y
    )) '() sequence))

; lisp 里的initial应该是放在整个处理过程的最后的，当然他是首先被执行的，和函数递归的方式相关
(define (append seq1 seq2) 
    (accumulate cons seq2 seq1))

(define (length sequence) 
    (accumulate (lambda (x y) (
        + 1 y
    )) 0 sequence))

(map square (list 1 2 3))
(append (list 1 2 3) (list 4 5 6 9))
(length (list 4 5 6))