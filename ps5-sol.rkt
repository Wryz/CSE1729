
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (catalan n)
  (define (factorial f)
    (accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) f))
  (/ (factorial (* n 2))
     (* (factorial (+ n 1))
        (factorial n))))
        
;;DOES NOT WORK (pls help me)
(define (leibniz-pi n)
  (- 1 (accumulate + 0 (lambda (x) (/ (expt -1 x) (+ 1 (* x 2)))) 1 (lambda (x) (+ x 1)) n)))

(define (accumulate-tr combiner null-value term a next b)
  (if (= a 0)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (fact n)
  (accumulate-tr * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (e-to-x x n)
  (if (= n 0)
      1
      (+ 1 (accumulate-tr + 0 (lambda (c) (/ (expt x c) (fact c))) 1 (lambda (c) (+ c 1)) n))))

(e-to-x 10 0)

(define (encode p)
  (let* ((x (car p))
         (y (cdr p)))
    (if (= x (max x y))
        (+ (expt x 2) x y)
        (+ (expt y 2) x))))

(define (decode z)
  (let ((f (floor (sqrt z))))
    (cond ((< (- z (expt f 2)) f)
           (cons (- z (expt f 2)) f))
          ((>= (- z (expt f 2)) f)
           (cons f (- z (expt f 2) f))))))

(define (sub-complex c d)
  (let* ((a (car c))
         (b (cdr c))
         (x (car d))
         (y (cdr d)))
    (cons (- a x)(- b y))))

(define (div-complex x y)
  (let* ((a (car x))
         (b (cdr x))
         (c (car y))
         (d (cdr y)))
    (cons (/ (+ (* a c)(* b d)) (+ (expt c 2) (expt d 2)))
          (/ (- (* b c) (* a d)) (+ (expt c 2) (expt d 2))))))

;;HAVE NOT COMPLETED PROBLEM #4
