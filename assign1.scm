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

(define (run1)
  (println "(my-and (= (/ 1 1) 0) (= (/ 1 0) 0))) would behave differently because in a regular \"and\" operation,
  the second boolean expression isn't evaluated if the first one is false, but in \"my-and\", both boolean expressions
  are evaluated in all cases.")
  )

;(run1)

; Keep the current min stored in the first variable
; Cycle the rest of them, overwriting either a or b depending on which is smaller
; All values have been checked when they are all the same
;(define (min5 a b c d e)
;  (if (= a b c d e)
;    a
;    (if (< b a)
;      (min5 b e b c d)
;      (min5 a e a c d)
;      )
;    )
;  )

(define (min5 a b c d e)
  (inspect (ppTable this))
  (if (eq? b +)
    a
    (if (< b a)
      (min5 b e + c d)
      (min5 a e + c d)
      )
    )
  )


(define (run2)
  (inspect (min5 5 4 3 2 1))
  (inspect (min5 2 3 1 4 5))
  )

(run2)

(define PI 3.14159265358979323846)

(define (cym val)
  
  (define (calcColorVal trigFunc a b c x)
    (int (* a 
            (+ (trigFunc (* b x)) 
               c)))
    )

    (define (calcCyanVal x)
    (calcColorVal cos 255 (* PI -0.005) 0 x)
    )
  
  (define (calcYellowVal x)
    (calcColorVal (lambda (x) (- (sin x))) 255 (* PI 0.01) 1 x)
    )
  
  (define (calcMagentaVal x)
    (calcColorVal cos 127.5 (* PI 0.015) 1 x)
    )
  
  (define (intToHex x)
    (define hexLetters (array "A" "B" "C" "D" "E" "F"))
    (define (getHexValue intValue)
      (if (> intValue 9)
        ; Get the corresponding hex letter
        (getElement hexLetters (- intValue 10))
        (string intValue)
        )
      )
    (define (intToHexIter y result power)
      (define current (/ y (^ 16 power)))
      (define remaining (% y (^ 16 power)))
      (define nextValue (getHexValue current))
      (define nextResult (string+ result nextValue))
      (if (= power 0)
        nextResult
        (intToHexIter remaining nextResult (- power 1))
        )
      )
    (define (padResult result)
      ; Zero-pad if the result is smaller than 2 characters
      (if (< (length result) 2)
        (string+ "0" result)
        result
        )
      )
  
    (padResult (intToHexIter x "" 1))
    )
    (string+ "#" (intToHex (calcCyanVal val)) (intToHex (calcYellowVal val)) (intToHex (calcMagentaVal val)))
  )

(define (run3)
  (inspect (cym 100))
  (inspect (cym 1))
  (inspect (cym 0))
  (inspect (cym 50))
  )

;(run3)

(define (root5 val)
  (define TOLERANCE 1.0E-6)

  (define (percentChange prev next)
    (abs (/ (- prev next) next))
    )

  (define (nextGuess x)
    (* 0.2 
       (+ (* 4 x) 
          (/ val
             (expt x 4))))
    )

  (define (root5Rec prev)
    (define next (nextGuess prev))
    ; Stop when the new guess is within 1.0E-6 percent of the previous guess
    (if (< (percentChange prev next) TOLERANCE)
      next
      (root5Rec next)
      )
    )
  (root5Rec 1.0)
  )

(define (run4)
  (inspect (root5 32))
  (inspect (root5 64))
  (inspect (root5 589e-12))
  )
;(run4)

(define (bico row col)
  (cond
    ((> col row) "Error: row too big")
    ((or (= col 0) (= col row)) 1)
    ; Add the number directly above the current number and the number above and to the left
    (else (+ (bico (- row 1) (- col 1)) (bico (- row 1) col)))
    )
  )

(define (run5)
  (inspect (bico 0 0))
  (inspect (bico 10 5))
  )
;(run5)

(define (curry f a) 
  (lambda (b) 
    (lambda (c) 
      (lambda (d) 
        (f a b c d)))))

(define (run6)
  (inspect ((((curry (lambda (a b c d) (* a b c d)) 2) 2) 2) 2))
  (inspect ((((curry (lambda (a b c d) a) 2) 3) 4) 5))
  )
;(run6)

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

(define (run7)
  (inspect (zorp 0 (lambda (n) (+ (^ n 3) (^ n 2) n))))
  )
;(run7)

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
  (define (halveIter current prev remaining numDoubles)
    (cond
      ; Add the remaining doubles
      ((= current remaining) (+ numDoubles prev))
      ; Once doubling the number makes it greater than the remaining value, start over,
      ; keeping track of how many doubles we've done so far
      ((> (double current) remaining) (halveIter 1 0 (- remaining current) (+ numDoubles prev)))
      (else (halveIter (double current) current remaining numDoubles))
      )
    )
  (halveIter 1 0 num 0)
  )

(define (run8)
  (inspect (egypt/ 1960 56))
  (inspect (egypt/ 2000 20))
  )
;(run8)


(define (mystery numTerms augend fnNumerator fnDenominator)
  (define (mysteryIter store n)
    (if (= n 0)
      ; Add the augend once we reach the end
      (+ augend (/ (fnNumerator n) store))
      (mysteryIter (+ (fnDenominator n) (/ (fnNumerator n) store)) (- n 1)))
    )
  (mysteryIter (real (fnDenominator numTerms)) (- numTerms 1))
)

(define (run9)
  ; 13 is the minimum number of iterations needed to get the number to converge
  (inspect (mystery 13 1 (lambda (n) 1) (lambda (n) 
                                         (if (= (% n 3) 1)
                                           (+ (/ n 3) n)
                                           1
                                           ))))
  )
;(run9)


(define (ramanujan numIters)
  (define (ramanujanRec n)
    (if (<= n numIters)
      (sqrt 
        (+ 
          (+ 6 n) 
          (* (+ 2 n) 
             (ramanujanRec (+ n 1)))))
      0
      )
    )
    (ramanujanRec 0)
  )

(define (iramanujan numIters)
  (define (iramanujanIter n store)
    (if (< n 0)
      store
      (iramanujanIter (- n 1) 
                      (sqrt 
                        (+ 
                          (+ 6 n) 
                          (* (+ 2 n) store))))
      )
    )
  (iramanujanIter numIters 0)
  )

(define (run10)
  ; 33 Is the minimum number of iterations needed to get the number to converge
  (inspect (ramanujan 33))
  (inspect (iramanujan 33))
  )
;(run10)

(println "assignment 1 loaded!")
