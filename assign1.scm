(define (author)
  (println "AUTHOR: Austin Schey aeschey@crimson.ua.edu")
  )

(define (exprTest # $expr target)
  (define result (catch (eval $expr #)))
  (if (error? result)
    (println $expr " is EXCEPTION: " (result'value)
             " (it should be " target ")")
    (println $expr " is  " result
             " it should be " target ")")
    )
  )


(define x 50)
  (define a 5)
    (define (my-and a b)
        (if (true? a)
            b
            #f
        )
    )
    ;(inspect (and (= (/ 1 1) 0) (= (% a 0) 1)))
    ;(inspect (my-and (= (/ 1 0) 1) (= (% a 0) 1)))

 (define (min5 a b c d e)
  (cond
    ((> a b) (min5 b a c d e))
    ((> a c) (min5 c b a d e))
    ((> a d) (min5 d b c a e))
    ((> a e) (min5 e b c d a))
    (else a))
  )
;(inspect (min5 5 4 3 2 1))

(define PI 3.14159265358979323846)

(define (cym val)
  (string+ (intToHex (calcCyanVal val)) (intToHex (calcYellowVal val)) (intToHex (calcMagentaVal val)))

  (define (calcColorVal a b c x)
    (int (* a 
            (+ (cos (* b x)) 
               c))))
    (define (calcCyanVal x)
    (calcColorVal 255 (* PI -0.005) 0 x)
    )
  
  (define (calcYellowVal x)
    (calcColorVal 127.5 (* PI 0.02) 1 x)
    )
  
  (define (calcMagentaVal x)
    (calcColorVal 127.5 (* PI 0.015) 1 x)
    )
  
  (define (intToHex x)
    (define hexLetters (array "A" "B" "C" "D" "E" "F"))
    (define (getHexValue intValue)
      (if (> intValue 9)
        (getElement hexLetters (- intValue 10))
        (string intValue)
        )
      )
    (define (intToHexRec y result)
      (define current (% y 16))
      (define remaining (/ y 16))
      (define nextValue (getHexValue current))
      (define nextResult (string+ result nextValue))
      (if (= remaining 0)
        nextResult
        (intToHexRec remaining nextResult)
        )
      )
    (define (padResult result)
      (if (< (length result) 2)
        (string+ "0" result)
        result
        )
      )
  
    (padResult (intToHexRec x ""))
    )
  )

(define test 100)
;(inspect (calcCyanVal test))
;(inspect (calcYellowVal test))
;(inspect (calcMagentaVal test))
;(inspect (cym test))

(define (root5 val)
  (define (nextGuess x)
    (* 0.2 
       (+ (* 4 x) 
          (/ val
             (expt x 4)))))

  (define (root5Rec prev)
    (define next (nextGuess prev))
    (if (< (abs (/ (- prev next) next)) 1.0E-6)
      next
      (root5Rec next)
      )
    )
  (root5Rec 1)
  )

;(inspect (root5 25.755))

(define (bico row col)
  (if (or (= col 0) (= col row))
    1
    (+ (bico (- row 1) (- col 1)) (bico (- row 1) col))
    )
  )

;(inspect (bico 4 2))

(define (curry f a) 
  (lambda (b) 
    (lambda (c) 
      (lambda (d) 
        (f a b c d)))))

;(inspect ((((curry (lambda (w x y z) (+ w x y z)) 1) 1) 1) 1))

(define (zorp i f)
  (define (calcZorp i1 i2 i3)
    (+ i1 
       (/ (^ (- i1 i2) 2) 
          (+ (- i3 (* 2 i2)) 
             i1)))
    )

  (define (zorpIter counter i1 i2 i3)
    (if (> counter i)
      i1
      (zorpIter (+ counter 1) (calcZorp i1 i2 i3) i1 i2)
      )
    )
  (if (< i 3)
    (f i)
    (zorpIter 3 (f 2) (f 1) (f 0))
    )
  )

;(inspect (zorp 10 (lambda (n) (+ (^ n 3) (^ n 2) n))))

(define (egypt/ divisor dividend)
  (define (egyptIterDouble a b)
    (if (> b divisor)
      (egyptIterHalve a b divisor 0)
      (egyptIterDouble (double a) (double b))
      )
    )

  (define (egyptIterHalve a b c d)
    (cond 
      ((< a 1) d)
      ((<= b c) (egyptIterHalve a b (- c b) (+ a d)))
      (else (egyptIterHalve (halve a) (halve b) c d))
      )
    )

  (egyptIterDouble 1 dividend)
  )
  

(define (double a)
  (+ a a)
  )

(define (halve num)
  ;Only called on one digit at a time
  (define (isEven n)
    (cond
      ((= n 0) #t)
      ((= n 1) #f)
      (else (isEven (- n 2)))
      )
    )
  (define (stringNum n) 
    (string+ "0" (string n))
    )
  (define (halveIter remaining result)
    (cond
      ((= (length remaining) 1) (int result))
      (else
         (define first (int (car remaining)))
         (define second (int (cadr remaining)))
         (define nextRemaining (cdr remaining))
         (define nextDigitCheck
           (cond
             ((< second 2) 0)
             ((< second 4) 1)
             ((< second 6) 2)
             ((< second 8) 3)
             (else 4)
             )
           )
         (define nextDigit
           (if (isEven first)
             nextDigitCheck
             (+ nextDigitCheck 5)
             )
           )
         (halveIter nextRemaining (string+ result nextDigit))
        )
      )
    )
    
  (halveIter (stringNum num) "")
  )

;(inspect (halve 30))
;(inspect (egypt/ 1960 56))

(define (mystery numTerms augend fnNumerator fnDenominator)
  (define (mysteryIter store n)
    (if (= n 0)
      (+ augend (/ (fnNumerator n) store))
      (mysteryIter (+ (fnDenominator n) (/ (fnNumerator n) store)) (- n 1)))
    )
  (mysteryIter (real (fnDenominator numTerms)) (- numTerms 1))
)
;(inspect (mystery 1000000 2 (lambda (n) 1) (lambda (n) n)))

(define (ramanujan numIters)
  (define (ramanujanRec n)
    (if (< n numIters)
      (sqrt (+ (+ 6 n) (* (+ 2 n) (ramanujanRec (+ n 1)))))
      0
      )
    )
    (ramanujanRec 0)
  )
(inspect (ramanujan 10))

(define (iramanujan numIters)
  (define (iramanujanIter n store)
    (if (< n 0)
      store
      (iramanujanIter (- n 1) (sqrt (+ (+ 6 n) (* (+ 2 n) store))))
      )
    )
  (iramanujanIter (- numIters 1) 0)
  )
(inspect (iramanujan 10))
