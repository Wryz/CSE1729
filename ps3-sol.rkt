#lang racket
;1a Harmonic Numbers
(define (harmonic x)
  (if (= x 1)
      1
      (+ (/ 1 x) (harmonic (- x 1)))))

;1b Euler Estimate
(define (Eulerest x)
  (abs (- (harmonic x) (log x))))

;2 Primality tes
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (and (> n 1)
       (not (divisors-upto (- n 1)))))

; 2 Count Primes
(define (count-primes m)
  (cond ((= m 1) 0)
        ((prime? m) (+ 1 (count-primes (- m 1))))
        (else (count-primes (- m 1)))))

; 3 Relatively Prime FINISH
(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-both k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))

(define (count-rel-prime n)
  (define (helper a b)
    (cond ((= b 1) 1)
           ((rel-prime a b)
            (+ 1 (helper a (- b 1))))
           (else (helper a (- b 1)))
           ))
  (define (helper2 n)
    (if (= n 1) 1
        (+ (helper n (- n 1))
           (helper2 (- n 1)))
        ))
  (helper2 n))

; 4a Lucas Numbers
(define (lucas n)
  (cond ((= n 0) 2)
        ((= n 1) 1)
        ((> n 1) (+ (lucas (- n 1))
                    (lucas (- n 2))))))

; 4b Lucas Number Ratios
(define (Lucas-ratio n)
  (/ (lucas n)
     (lucas (- n 1))))

(define (fib n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               ((> n 1) (+ (fib(- n 1))
                           (fib (- n 2))))))

(define (Fibonacci-ratio n)
  (/ (fib n)
     (fib (- n 1))))

; 4c
(define (fast-Lucas-help n k lucas-a lucas-b)
  (if (= n k)
      lucas-a
      (fast-Lucas-help n (+ k 1) (+ lucas-a lucas-b) lucas-a)))

(define (fast-Luca n)
  (fast-Lucas-help n 1 1 2))

;; This function represents the table shown in the PDF.
;; Simply "hard-code" the number of recursive call you believe
;; take place for inputs 3 through 6
(define (rec-call-lucas k)
    (cond ((= k 1)  0)
          ((= k 2)  2)
          ((= k 3)  4)
          ((= k 4)  8)
          ((= k 5)  14)
          ((= k 6)  24)
    ))

;; Do the same for the fast-lucas-helper
(define (rec-call-fast-lucas-helper k)
    (cond ((= k 1)  0)
          ((= k 2)  1)
          ((= k 3)  2)
          ((= k 4)  3)
          ((= k 5)  4)
          ((= k 6)  5)
    ))
;5a Half-Companion Pells
(define (H n)
  (if (= n 0)
      1
      (+ (H (- n 1))
         (* 2 (P (- n 1))))))

;5b Pells﻿
(define (P n)
  (if (= n 0)
      0
      (+ (H (- n 1))
         (P (- n 1)))))

;5c (t n)
(define (t n)
  (if (= 0 (modulo n 2)) ;;if remainder is 0
      (* 2 (expt (P n) 2)) ;;2 x (P^2)
      (expt (H n) 2)))

;5d (s n)
(define (s n)
  (* (H n) (P n)))

;5e Triangular Squares
(define (tri-square n)
  (/ 2 (* (t n) (+ (t n) 1))))

;5f (square-tri n)
(define (square-tri n)
  (expt (s n) 2))

;6a Golden Ratio by continued fractions
(define (golden n)
  (if (= n 1)
      2
      (+ 1 (/ 1 (golden (- n 1))))))

;6b Golden Ratio by continued square root
(define (golden-sqrt n)
  (if (= n 0)
      1
      (sqrt (+ 1 (golden-sqrt (- n 1))))))

;7 explain
(define (explain-interval-sum)
  (define a "One can never do  an induction on both inputs at once.")
  (define b "The base case isn't quite right. It needs to be updated to account for the two inductive calls.")
  (define c "The inductive case should be adding three things together.")
  (define d "The predicate to recognize the base case is wrong. One can go from m > n to m < n without ever seeing n = m.")
  (define e "I have no idea.")
  d)


; 8 Ackermann Function
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (ack (- m 1) 1))
        ((and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1))))))

; 9 Catalan numbers
(define (catalan n)
  (if (= n 0)
      1
      (/ (factorial (* n 2))
         (* (factorial (+ n 1))
            (factorial n)))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
      )
  )
