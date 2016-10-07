(define (f x)
  (define a (+ x 1))
  (define b (* a a))
  (define (g c) (* c c))
  (+ (* a a) (* (g a) a) (* (g b) b))
  )

(define (ff x)
  ((lambda (a g) ((lambda (b) (+ (* a a) (* (g a) a) (* (g b) b))) (* a a))) (+ x 1) (lambda (c) (* c c)))
  )

;(inspect (f 5))
;(inspect (ff 5))

(define (nsq a)
  (define i (+ 1 1))
  (define x (+ a 1))
  (define y (+ x x))
  (define j (+ 2 2))
  (* y y x i j)
  )

(define (nssq a)
  ((lambda (x) ((lambda (y) (* y y)) (+ x x))) (+ a 1))
  )

(inspect (nsq 2))
(inspect (nssq 2))

((lambda (i) ()) (+ 1 1))
((lambda (x i) ()) (+ a 1) (+ 1 1))

((lambda (y) ()) (+ x x))
((lambda (j y) ()) (+ 2 2) (+ x x))
((lambda (j y) (* y y x i j)) (+ 2 2) (+ x x))

((lambda (x i) ((lambda (j y) (* y y x i j)) (+ 2 2) (+ x x))) (+ a 1) (+ 1 1))

(define x (+ 1 1))
(define y (+ x x))
(+ x y)
((lambda (x) ()) (+ 1 1))
((lambda (y) (+ x y)) (+ x x))
((lambda (x) ((lambda (y) (+ x y)) (+ x x))) (+ 1 1))
