(library (test finite-diff)
  (export run-all) 
  (import (lib finite-diff)
          (only (test utils) make-test-set test-within-bound?)
          (rnrs)
          (only (chezscheme) format eval-when))

  (define run-all
    (make-test-set unitary-function-1
                   ((f1 (lambda (x) (* (sin (* 10 x)) (exp (/ x -2)))))
                    (df1 (lambda (x) (+ (* 10 (cos (* 10 x)) (exp (/ x -2)))
                                        (* (/ -1 2) (sin (* 10 x)) (exp (/ x -2))))))
                    (df1-aprox (lambda-derive-finite-diff f1  0.0001 x x)))
                   ((test-within-bound? 0.01 (df1-aprox 0 ) (df1 0))
                    (test-within-bound? 0.01 (df1-aprox 1 ) (df1 1))
                    (test-within-bound? 0.01 (df1-aprox -1) (df1 -1))))))
