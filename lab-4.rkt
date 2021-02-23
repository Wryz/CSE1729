#lang racket
;;Q 1
(define (windchill W F)
  (let ((x (sqrt W)))
       (+ 1.05 (* .93 F) (- (* 3.65 W))
          (* 3.62 x) (* .103 F x) (* .0439 W W))))

;;Q 2
(define (iradius A B C)
  (let ((S (/ (+ A B C) 2)))
       (sqrt (/ (* (- S A) (- S B) (- S C)) S))))

;;Q 3
(define (pressure h)
  (let ((p0 101325))
       (let ((L .0065))
            (let ((T0 288.15))
                 (let ((g 9.80665))
                      (let ((M .0289644))
                           (let ((R 8.31447))
                                (* p0 (expt (- 1 (/(* L h) T0)) (/ (* g M) (* R L)))))))))))

;;Q 4
(define (compose f g)
  (define (composed a)
    (f (g a)))
  composed)

;;Q 5
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))