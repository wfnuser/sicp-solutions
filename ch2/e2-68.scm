(define (make-leaf symbol weight) 
    (list 'leaf symbol weight))
(define (leaf? object) 
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right) 
    (list left 
        right 
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree)) 

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree)) 
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch
                (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                            (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch))))
    )
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-tree 
    (make-code-tree 
        (make-leaf 'A 4)
        (make-code-tree
         (make-leaf 'B 2)
         (make-code-tree
            (make-leaf 'D 1)
            (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree) 
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


; TODO: try to use DFS
; (define (encode-symbol symbol tree)
;     (define (search symbol tree path)
;         (let ((left (left-branch tree) (right (right-branch tree))))
;             (if (leaf? left)
;                 (if (eq? (symbol-leaf left) symbol)
;                     (cons 0 path)
;                     '()
;                 )
;                 (if (null? (search symbol left (cons 0 path)))
;                     (if (leaf? right)
;                         (if (eq? (symbol-leaf right) symbol)
;                             (cons 1 path)
;                             '()
;                         )
;                         (search symbol right (cons 1 path))
;                     )
;                 )
;             )
;             '()
;         )
;     )
;     search symbol tree '()
; )

; (encode-symbol 'A sample-tree)

(define (encode-symbol symbol tree)
    (cond ((leaf? tree)
        '())
        ((correct? symbol (left-branch tree))
            (cons 0 (encode-symbol symbol (left-branch tree))))
        ((correct? symbol (right-branch tree))         
            (cons 1 (encode-symbol symbol (right-branch tree))))
        (else                                                 
            (error "This symbol not in tree: " symbol))))

(define (correct? symbol tree)
    (define (find target list)
        (if (null? list)
            #f
            (if (eq? (car list) target)
                #t
                (find target (cdr list))
            )
        )
    )
    (find symbol (symbols tree))
)

(encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree)
