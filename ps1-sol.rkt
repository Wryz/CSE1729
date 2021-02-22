;Question 1
;Part a
(* (+ 22 42) (* 54 99))
;Part b
(* 99 (* 54 (+ 22 42)))
;Part c
(+ (* 64 102) (* 16 (/ 44 22)))

;Define the limerick variable here
(define limerick ( + (* 5 11) (/ (+ 12 144 20 (* (sqrt 4) 3)) 7)))

;Question 3
;Part a (we are giving you the first one... ;-)
(define (inc x) (+ x 1) )

;Part b
(define (inc2 x) (inc (inc x)))

;Part c
(define (cube x) (expt x 3))

;Part d
(define (p x) (expt (+ (expt x 5) (* 16 (expt x 4)) (* 22 (expt x 3)) x 9) 2))

;Part e
(define (ninth x) (cube (cube x)))

;Part f
(define (eighty-first x) (ninth (ninth x)))

;Replace the ... with the proper function body
(define (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2) (modulo(- 11 (modulo (+
                                                         (* 10 x10)
                                                         (* 9 x9)
                                                         (* 8 x8)
                                                         (* 7 x7)
                                                         (* 6 x6)
                                                         (* 5 x5)
                                                         (* 4 x4)
                                                         (* 3 x3)
                                                         (* 2 x2)) 11)) 11))

;Replace the ... with the proper function body
;Be sure to include your implemenation of isbn10-checkdigit here too
(define (is-isbn10? x10 x9 x8 x7 x6 x5 x4 x3 x2 x1)
  (if (= (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2 x1)

;Replace the ... with the proper function body
(define (fspiral theta)
  (expt 1.618 (* theta (/ 2 3.142))))
