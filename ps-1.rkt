;Question 1
;Part a
(* (+ 22 42)
   (* 54 99))
;Part b
(* (* (+ 22 42)
      54)
   99)
;Part c
(+ (* 64 102)
   (* 16
      (/ 44 22)))


(define limerick (+ (/ (+ 12 144 20 (* 3 (sqrt 4)))
                       7)
                    (* 5 11)))

;Question 3
;Part a
(define (inc x) (+ x 1))
;Part b
(define (inc2 x) (inc (inc x)))
;Part c
(define (cube x) (* x x x))
;Part d
(define (q x)
        (+ 9 (* x
                (+ 1 (* x x
                        (+ 22 (* x
                                 (+ 16 x))))))))
(define (p x) (* (q x) (q x)))
;Part e
(define (ninth x)
  (cube (cube x))) 
; Part f
(define (eighty-first x)
  (ninth (ninth x)))

(define (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2)
  (modulo (- 11
     (modulo (+ (* x10 10)
                (* x9 9)
                (* x8 8)
                (* x7 7)
                (* x6 6)
                (* x5 5)
                (* x4 4)
                (* x3 3)
                (* x2 2))
             11)) 11))

(define (is-isbn10? x10 x9 x8 x7 x6 x5 x4 x3 x2 x1)
  (= (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2)
     x1))

;Replace the ... with the proper function body
(define (fspiral theta)
        (expt 1.618 (* theta (/ 2 3.142))))
