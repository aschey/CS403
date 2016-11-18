(define (gcd a b) (if (= b 0) a (gcd b (% a b))))
(define (rprime? x y) (= (gcd x y 1)))
(define (rand-update x) (randSeed x) (randomInt))
(define rints (scons seed (smap rand-update rints)))
(define cesaro (smap-pairs rprime? rints))
(define (monte-carlo experiments passed failed)
  (define ratio (/ (real passed) (+ passed failed)))
  (if (scar experiments)
    (scons ratio (monte-carlo (scdr experiments) (+ passed 1) failed))
    (scons ratio (monte-carlo (scdr experiments) passed (+ 1 failed)))
    )
  )

(inspect (sref (smap (lambda (x) (^ (/ 6 x) 0.5)) (monte-carlo cesaro 1 0)) 8000))

; install and remove called per thread, set is global
; use busy wait (loop?) in install, call install again
