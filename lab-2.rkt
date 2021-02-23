(define (divisible? x y)
    (zero? (modulo x y)))

(define (fizzbuzz x)
    (cond ((divisible? x 15) "fizzbuzz")
          ((divisible? x 3) "fizz")
          ((divisible? x 5) "buzz")
          (else x)))

(define (fizz x)
  (if (= 0 (modulo x 3)) "fizz" ""))

(define (buzz x)
  (if (= 0 (modulo x 5)) "buzz" ""))

(define (fizzbuzz2 x)
  (if (or (= 0 (modulo x 3))
          (= 0 (modulo x 5)))
      (string-append (fizz x) (buzz x)) 
      x))


(define pi 3.14159)

(define (piecewise x)
  (cond ((< x (- pi)) (- (- x) pi))
        ((<= x (* 2 pi)) (sin x))
        (else (- x (* 2 pi)))))


(- (* 2 pi)  (* 2 pi))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;; Addition without using +
(define (add n m)
  (cond ((= n 0) m)
        (else (inc (add (dec n) m)))))

(add 5 10)

;; Alternative implementation of +
(define (add2 n m)
  (cond ((= n 0) m)
        (else (add2 (dec n) (inc m)))))

(add2 5 10)

;; Multiplication without *
(define (mult n m)
  (cond ((= n 0) 0)
        (else (add m (mult (dec n) m)))))

(mult 4 10)

;; Alternative for multiplication without *
(define (mult2 n m)
  (define (aux n m acc)
    (cond ((= n  0) acc)
          (else (aux (dec n) m (add acc m)))))
  (aux n m 0))

(mult2 4 10)

;; exponentiation (base^n)
(define (power x n)
  (cond ((= n 0) 1)
        (else (mult x (power x (dec n))))))

(power 2 8)

;; Fast exponentiation (repeated squaring)
(define (raise x n)
  (define (doit a n)
    (cond ((= (modulo n 2) 0) (* a a))
          (else (* a a x))))                              
  (cond ((= n 0) 1)
        (else (doit (raise x (floor (/ n 2))) n))))

(raise 2 8)

;; Sum of even values (linear -1)
(define (sumEven n)
  (cond ((= n 0) 0)
        ((= (modulo n 2) 0) (+ n (sumEven (- n 1))))
        (else (sumEven (- n 1)))))

(sumEven 10)

;; Sum of odd values (linear -2)
(define (sumOdd n)
  (cond ((<= n 0) 0)
        ((= (modulo n 2) 1) (+ n (sumOdd (- n 2))))
        (else (sumOdd (- n 1)))))

(sumOdd 10)

;;Product of fractions (from prior)
(define (h-product n)
  (cond ((= n 1) 1)
        (else (* (- 1 (/ 1 n)) (h-product (- n 1))))))


(define (divides a b)
  (= 0 (modulo b a)))

;; Counting divisors (from prior)
(define (divisors-upto k n)
  (cond ((= k 0) 0)
        ((= n 0) 0)
        ((= k 1) 1)
        ((divides k n) (+ 1 (divisors-upto (- k 1) n)))
        (else (divisors-upto (- k 1) n))))

(define (divisors n) (divisors-upto n n))

;;subfactorial
(define (subfact n)
  (cond ((= n 0) 1)
        ((= n 1) 0)
        (else (* (- n 1)
                 (+ (subfact (- n 1)) (subfact (- n 2)))))))
;; given
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

;; cos Taylor expansion
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (new-cos x k)
     (if (= k 0)
         1.0
         (let ((p (* 2 k)))
           (+ (* (expt -1 k)
                 (expt x p)
                 (/ 1 (factorial p)))
              (new-cos x (- k 1))))))
