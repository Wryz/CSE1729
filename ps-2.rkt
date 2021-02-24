#lang racket
;Problem 1a
(define (fizzbuzz x)
  (cond ((and (= (modulo x 3) 0)
              (= (modulo x 5) 0)) "fizzbuzz")
        ((= (modulo x 5) 0) "buzz")
        ((= (modulo x 3) 0) "fizz")
        (else x)
        )
  )

;Problem 1b
(define (fizz x)
  (if (= (modulo x 3) 0) "fizz" ""))
(define (buzz x)
  (if (= (modulo x 5) 0) "buzz" ""))

(define (fizzbuzz2 x)
  (string-append (fizz x) (buzz x))
  )

;Problem 2
(define (piecewise x)
  (cond ((> x (* 2 3.142)) (- x (* 2 3.142)))
        ((and (>= x -3.142) (>= (* 2 3.142) x)) (sin x))
        ((> -3.142 x) (- (* -1 x) -3.142))
        )
  )

;Problem 3
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

(define (add n m)
  (cond ((= n 0) m)
        ((= m 0) n)
        ((> n 0) (add (dec n)(inc m)))
        )
  )

;Problem 4
(define (mult b n)
  (cond ((or (= b 0) (= n 0)) 0)
        ((> n 0) (add b (mult  b (dec n))))
        )
  )

;Problem 5
(define (power b n)
  (cond ((= n 0) 1)
        ((> n 0) (mult b (power b (dec n))))
        )
  )

;Problem 6
(define (raise x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        ((= (modulo (floor n) 2) 0) (raise x (/ n 2)))
        (else (* (raise x (/ n 2)) n))
              )
        )

;Problem 7a
(define (sumEven n)
  (cond ((<= n 0) 0)
        ((not (= (modulo n 2) 0)) (sumEven (- n 1)))
        (else (+ n (sumEven (- n 2))))
        )
  )

;Problem 7b
(define (sumOdd n)
  (cond ((<= n 0) 0)
        ((= (modulo n 2) 0) (sumOdd (- n 1)))
        (else (+ n (sumOdd (- n 2))))
        )
  )

;Problem 8
(define (h-product k)
  (if (= k 1)
      1
      (* (- 1 (/ 1 k))
         (h-product (- k 1)))))

;Problem 9
(define (divides a b) (= (modulo b a) 0))

(define (divisors-upto n k)
  (cond ((= k 0) 0)
        ((= n 0) 0)
        ((= k 1) 1)
        ((divides k n)
         (+ 1 (divisors-upto n (- k 1))))
        ((not (divides k n))
         (divisors-upto n (- k 1)))
        )
  )

(define (divisors n) (divisors-upto n n))

;Problem 10
(define (subfact n)
  (cond ((= n 0) 1)
        ((= n 1) 0)
        ((> n 1) (*
                  (- n 1)
                  (+ (subfact (- n 1)) (subfact (- n 2))))
                 )
        )
  )

;Problem 11
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      )
  )

(define (new-cos x n)
  (if (= n 0)
      1
      (let ((p (* 2 n)))
        (+ (* (expt -1 n)
              (expt x p)
              (/ 1 (factorial p)))
           (new-cos x (- n 1))))))


  
