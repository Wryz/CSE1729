;Problem 1a: usd-to-euro
(define (usd-to-euro a)
 (* a 0.82))

;Problem 1b: euro-to-yen
(define (euro-to-yen f)
  (* f 126.01))

;Problem 1c: usd-to-yen
(define (usd-to-yen y)
  (euro-to-yen (usd-to-euro y)))
(usd-to-yen 250)

;Problem 2a: Initialize e
(define e 2.71828)

;Problem 2b: tanh function
(define (tanh x)
 (/ (- (expt e (* 2 x)) 1) (+ (expt e (* 2 x)) 1) ))

;Problem 3a: Matrix
(define (det2x2 a b c d)
  (- (* a d)
     (* b c)))
(det2x2 2 -4 -6 12)

;Problem 3b: invertible
(define (invertible? a b c d)
  (not (= (det2x2 a b c d) 0 )))
(invertible? 2 -4 -6 12)

;Problem 3ci:
(define (prod-inv-direct? a1 b1 c1 d1 a2 b2 c2 d2)
  (not (= 0
          (det2x2 (+ (* a1 a2)
                  (* b1 c2))
               (+ (* a1 b2)
                  (* b1 d2))
               (+ (* c1 a2)
                  (* d1 c2))
               (+ (* c1 b2)
                  (* d1 d2))))))
(prod-inv-direct? 2 -4 -6 12 2 -4 -6 12)

;Problem 3cii:
(define (prod-inv-indirect? a1 b1 c1 d1 a2 b2 c2 d2)
  (not (= (* (det2x2 a1 b1 c1 d1)
          (det2x2 a2 b2 c2 d2))
       0)))
(prod-inv-indirect? 2 -4 -6 12 2 -4 -6 12)
