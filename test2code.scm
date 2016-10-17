(define (countCons x)
  (cond
    ((atom? x) 0)
    (else (+ 1 (countCons (car x))
             (countCons (cdr x)))
          )
    )
  )

;lookup, create, insert
;two parallel lists- variables, values

(define (conss a b) (lambda (f) (if (f) a b)))
(define (cdrr c) (c (lambda () #f)))
;(inspect (cdrr (conss 1 (list 2 3))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define (inc x) (list x '+ 1))
(define base 0)

;(inspect ((three inc) base))

;(inspect (map (lambda (x) (map (lambda (y) (list x y)) '(1 2 3))) '(8 7 6)))

(define (accumulate op base l)
  (cond
    ((null? l) base)
    (else (op (car l) (accumulate op base (cdr l))))
    )
  )

(define (flatten l)
  (accumulate append nil l)
  )

;(inspect (flatten (list (list 1 2) (list 3 4))))

(define l (list 1 2 3))
(define (f x) (+ x 1))
;(inspect (accumulate (lambda (x y) (cons (f x) y)) nil l))

;(inspect (accumulate (lambda (x y) (+ 1 y)) 0 l))

(inspect (map (lambda (x) (+ 1 (apply + (cdr m)))) m))
