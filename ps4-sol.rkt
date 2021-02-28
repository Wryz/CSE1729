;;1
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (harmonic n)
  (sum (lambda (x) (/ 1 x)) 1 (lambda (x) (+ x 1)) n))

;;2a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;;2b
(define (wallis-pi n)
  (define (inc x) (+ x 1))
    (define (term x)
      (if (even? x)
          (/ (+ x 2.0) (+ x 1.0))
          (/ (+ x 1.0) (+ x 2.0))))
    (product term 1.0 inc n))

;;3
(define (frac-sum-range f g n x)
  (cond ((< x n) 0)
        ((= 0 (g n))
         (frac-sum-range f g (+ n 1) x))
        (else (+ (/ (f n) (g n))
                 (frac-sum-range f g (+ n 1) x)))))

(define (frac-sum f g n) (frac-sum-range f g (- n) n))
  

;;4a
(define (der f h)
  (define (ret x)
    (/ (- (f (+ x h)) (f x)) h))ret)

;;4b
(define (der-n f n h)
  (if (= n 0) f (der-n (der f h) (- n 1) h)))

;;5
(define (newton f x n)
  (define dF (der f .01))
  (define (iteration x k)
    (let ((2x (- x (/ (f x) (dF x)))))
      (if (= k 0) x (iteration 2x (- k 1)))))
  (iteration x n))

;;6
(define (sum-term term a b)
  (if ( > a b)
      0
      (+ (term a) (sum-term term (+ a 1) b ))))

(define (simpson-integrate f a b n)
  (let ((m (* 2 n))
        (h (/ (- b a) (* 2 n))))
    (define (even x) (= (modulo x 2) 0))
    (define (odd x) (not (even x)))
    (define (y k)
      (f (+ a (* k h))))
    (define (s-term k)
      (cond ((= k 0) (y k))
            ((= k m) (y k))
            ((even k) (* 2 (y k)))
            ((odd k) (* 4 ( y k)))))
    (* (/ h 3) (sum-term s-term 0 m))))
