
(define (square-pair x)
  (cons x (expt x 2)))

(define (rev p)
  (cons (cdr p) (car p)))

(define (c->p p)
  (cons (sqrt (+ (expt (car p) 2) (expt (cdr p) 2)))
        (atan (/ (cdr p) (car p)))))

(define (p->c p)
  (let ((x (* (car p) (cos (cdr p)))))
    (let ((y (* (car p) (sin (cdr p)))))
      (cons x y))))

(define (y p1 p2)
  (let* ((m (/ (- (cdr p2) (cdr p1))
               (- (car p2) (car p1))))
         (b (- (cdr p1) (* m (car p1)))))
      (lambda (x) (+ (* m x) b))))

(y (cons 5 5) (cons 1 2))

