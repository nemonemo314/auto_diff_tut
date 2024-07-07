(eval-when (compile)
  (case-sensitive #t)
  (optimize-level 2)) 
;;(library (test finite-diff)  (export run-finite-diff-tests))
(import (lib finite-diff)
        (test utils)
        (rnrs)
        (only (chezscheme) format eval-when))

;; (define (run-finite-diff-tests)
;;   (test-function-1))




(define (test-function-1)
  (let ([f1 (lambda (x) (* (sin (* 10 x)) (exp (/ x -2))))]
        [f1-in (list -1 0 1 )]
        [f1-expected (list -14.2824 10 -4.92424)])
    (let ([f1-res (map (lambda (i) ((lambda-derive-finite-diff f1  0.00001 x x) i)) f1-in)])
      (assert (for-all (lambda (v gd) (within-bound? 0.001 v gd)) f1-res f1-expected)))))

(make-test-set univariate-finite-difference
               ((f1 (lambda (x) (* (sin (* 10 x)) (exp (/ x -2)))))
                (df1 (lambda (x)
                       (+ (* 10 (cos (* 10 x)) (exp (/ x -2)))
                          (* (/ -1 2) (sin (* 10 x)) (exp (/ x -2))))))
                (df1-aprox (lambda-derive-finite-diff f1  0.0001 x x)))

               ((test-within-bound? 0.01 (df1-aprox 0) (df1 0))
                (test-within-bound? 0.0000001 (df1-aprox 1) (df1 1))
                (test-within-bound? 0.01 (df1-aprox -1)  (df1 -1))))


