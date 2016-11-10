(define scdr stream-cdr)
(define scons cons-stream)

(define (sref s n) (if (= n 0) (scar s) (sref (scdr s) (- n 1))))

(define (sregion s m n)
  (cond
    ((= m 0) (print "[... ") (svdisplay s n) (println " ...]"))
    (else (sregion (scdr s) (- m 1) n))
    )
  )

(define (svdisplay s n)
  (cond
    ((= n 1) (print (scar s)))
    ((> n 1) (print (scar s) " ") (svdisplay (scdr s) (- n 1)))
    (else nil)
    )
  )

(define (sdisplay s n) (print "[") (svdisplay s n) (println "...]"))
(define (sop op s t) (scons (op (scar s) (scar t)) (sop op (scdr s) (scdr t))))
(define (smap f s) (scons (f (scar s)) (smap f (scdr s))))
(define (spsum s) (scons (scar s) (sop + (scdr s) (spsum s))))

(define ones (scons 1 ones))
(define alt-ones (scons 1 (scons -1 alt-ones)))
(sdisplay alt-ones 10)
(define wholes (scons 0 (sop + ones wholes)))
(sdisplay wholes 10)
(define twos (scons 2 twos))
(define odds (scons 1 (sop + twos odds)))
(define pi-terms (sop (lambda (x y) (/ (* 4.0 x) y)) alt-ones odds))
(sdisplay pi-terms 10)
(define pi-psums (spsum pi-terms))
(sdisplay pi-psums 10)
; euler transform
(define (et s)
  (define s0 (sref s 0))
  (define s1 (sref s 1))
  (define s2 (sref s 2))
  (scons
    (- s2
       (/ (^ (- s2 s1) 2) (+ s0 (* -2 s1) s2)))
    (et (scdr s))
    )
  )

(define acc-ps (et pi-sums))

(define acc-acc-ps (et (et pi-sums)))

(define (tableau xf s)
  (scons s (tableau xf (xf s))))

(define hypersonic-ps (smap scar (tableau et pi-psums)))

(define (pairs s t)
  (scons
    (list (scar s) (scar t))
    (shuffle
      (pairs (scdr s) (scdr t))
      (smap (lambda (x) (list (scar s) x))
          (scdr t)
          )
      )
    )
  )

