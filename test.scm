(define (f x)
        (define y (- x 1))
        (define z (+ x 1))
        (+ (* x y) (* x z) (* y z))
        )

(define (ff x)
  ((lambda (y z) 
