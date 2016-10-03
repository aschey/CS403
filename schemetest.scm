(define (f x)
  (define a (+ x 1))
  (define b (* a a))
  (define (g c) (* c c))
  (+ (* a a) (* (g a) a) (* (g b) b))
  )

(define (ff x)
  ((lambda (a g) ((lambda (b) (+ (* a a) (* (g a) a) (* (g b) b))) (* a a))) (+ x 1) (lambda (c) (* c c)))
  )

(display (f 5)) (newline)
(display (ff 5))

