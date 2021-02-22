;Replace the ... with the body of the function
(define (pell n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((> n 1) (+ (* 2 (pell (- n 1)))
                              (pell (- n 2))))))

;Replace the ... with the funciton body
(define (find-pell n)
    (define (helper x n)
        (if (> (pell x) n)
            (pell (- x 1))
            (helper (+ x 1) n)))
        (helper 0 n))

;Replace the ... with the function body
(define (comp-pell n)
    (cond ((= n 0) 2)
          ((= n 1) 2)
          (else (+ (* 2 (comp-pell (- n 1))) (comp-pell (- n 2))))))

(define (sqrt-2-approx n)
    (/ (/ (comp-pell n) 2) (pell n)))

(define (viete n)
    (define (vietehelp num i)
        (let ((num (sqrt (+ 2 num))))
             (if (= i 0)
                 1
                 (* (/ num 2)
                    (vietehelp num (- i 1))))))
                (* (/ (sqrt 2) 2) (vietehelp (sqrt 2) (- n 1))))

;Complete the function skeleton below
(define (new-sqrt x n)
    (define (fraction y)
        (if (= y 0) 0
            (/ (- x 1) (+ 2 (fraction (- y 1))))
        )
    )
(+ 1 (fraction n))
)

;Replace the ...  with the proper function body
(define (m91 n)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))
