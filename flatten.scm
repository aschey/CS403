(define (flatten l)
  (cond 
    ((null? l) l)
    ((pair? (car l)) (append (flatten (car l)) (flatten (cdr l))))
    ((atom? (car l)) (append (list (car l)) (flatten (cdr l))))
    )
  )
;(inspect (flatten (list 1 2 (list 3 4))))

(define (fillter l pred)
  (cond 
    ((null? l) l)
    ((eq? (pred (car l)) #t) (cons (car l) (fillter (cdr l) pred)))
    (else (fillter (cdr l) pred))
    )
  )

;(inspect (fillter (list 1 2 3 4 5) (lambda (x) (= (% x 2) 0))))

(define (accumulate op base l)
  (cond 
    ((null? l) base)
    (else (op (car l) (accumulate op base (cdr l))))
    )
  )

;(inspect (accumulate * 1 (list 1 2 3)))
    

(define (sumOdd l)
  (accumulate + 0 (fillter (flatten l) odd?))
  )
;(inspect (sumOdd (list 1 2 3 4 5)))

(define (enumerate start end)
  (cond
    ((> start end) nil)
    (else (cons start (enumerate (+ start 1) end)))
    )
  )

;(inspect (enumerate 1 5))

(define (expand l val dir)
  (cond
    ((null? l) nil)
    ((eq? dir 'front) (cons (cons val (list (car l))) (expand (cdr l) val dir)))
    ((eq? dir 'back) (cons (cons (car l) (list val)) (expand (cdr l) val dir)))
    )
  )

(define (mapp l func)
  (cond
    ((null? l) nil)
    (else (cons (func (car l)) (mapp (cdr l) func)))
    )
  )
;(inspect (mapp (list 1 2 3 4 5) (lambda (x) (+ x 1))))

;(inspect (expand (list 1 2 3) 1 'back))

(define (collect1 n)
  (cond
    ((= n 0) nil)
    (else (mapp (enumerate 0 n) (lambda (x) (expand (enumerate 0 n) x 'front))))
    )
  )

;(inspect (collect1 10))

(define (type-tag datum)
  (car datum)
  )

(define (plus x y)
  (+ x 1)
  )

(define (apply-generic op a b)
  (define args (list a b))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (apply proc (map contents args))
      )
    )
  )

;(inspect (apply-generic 'plus (list 'test 1) (list 'test 2)))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a)
  (lambda (b)
    (lambda (f)
      (lambda (x)
        ((a f) ((b f) x))
        )
      )
    )
  )

(define (inc x) (+ x 1))

;(inspect (((((two add) two) zero) inc) 0))

(define h2 (lambda (x) ((if (pair? a) append cons) x x)))

;(inspect (accumulate h2 nil (list 1 2 3)))

(inspect (accumulate cons nil (list 1 2 3)))
