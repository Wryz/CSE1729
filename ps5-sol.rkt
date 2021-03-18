
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

(define (leibniz-pi x)
  (define (form n)
    (* (expt (- 1) n) (/ 1(+ 1(* 2 n)))))
  (define (next n)
    (+ n 1))
  (define (add x1 x2)
    (+ x1 x2))
  (* 4 (accumulate add 0 form 0 next (- x 1))))

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

(define (sum-quadratic-roots a b c)
  (sub-complex (cons 0 0) (div-complex b a)))

(define (prod-quadratic-roots a b c)
  (div-complex c a))

(define (sum-cubic-roots a b c d)
  (define p (cons 0 0))
  (let ((dived (div-complex b a)))
    (sub-complex p dived)))

(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))

(define (prod-cubic-roots a b c d)
  (define p (cons 0 0))
  (let ((dived (div-complex d a)))
    (sub-complex p dived)))