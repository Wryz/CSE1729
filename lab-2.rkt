#lang racket
(define (geom-series-np2 n)
    (if (= n 0)
    1
    (+ (/ 1 (expt 2 n))
        (geom-series-np2 (- n 1)))))

(define (num-digits n)
  (if (< n 10)
      1
      (+ 1 (num-digits (/ n 10)))))

(define (a n)
    (if (= n 1)
        2
        (* 2 (a (- n 1)))))

(define (num-ancestors n)
  (if (= n 1)
      2
      (+ (expt 2 n)
         (num-ancestors (- n 1)))))

(define (factorial n)
  (if(= n 0)
     1
     (* n (factorial (- n 1)))))

(define (n-choose-k n k)
  (cond ((< n k) 0)
        ((< k 0) 0)
        (else (/ (factorial n) (* (factorial (- n k)) (factorial k))))))

(define (pascals-triangle n k)
  (cond ((and (= n 0) (= k 0)) 1)
        ((< k 0) 0)
        ((< n k) 0)
        (else (+ (pascals-triangle (- n 1) k)
                 (pascals-triangle (- n 1) (- k 1))))))







