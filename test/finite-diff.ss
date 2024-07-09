(library (test finite-diff)
  (export run-all) 
  (import (lib finite-diff)
          (only (test utils) make-test-set test-within-bound?)
          (rnrs)
          (only (chezscheme) format eval-when))

  (define unitary-function-1
    (make-test-set unitary-function-1
                   ;; f(x) = sin(10 * x) + e^(x/2)
                   ;; df(x)/dx = 10 * cos(10 * x) * e^(x/2) - 1/2 * sin(10 * x) * e^(x/2)
                   ((f1 (lambda (x) (* (sin (* 10 x)) (exp (/ x -2)))))
                    (df1 (lambda (x) (+ (* 10 (cos (* 10 x)) (exp (/ x -2)))
                                        (* (/ -1 2) (sin (* 10 x)) (exp (/ x -2))))))
                    (df1-aprox (lambda-derive-finite-diff f1  0.0001 x x)))
                   ((test-within-bound? 0.01 (df1-aprox 0 ) (df1 0))
                    (test-within-bound? 0.01 (df1-aprox 1 ) (df1 1))
                    (test-within-bound? 0.01 (df1-aprox -1) (df1 -1)))))
  
  (define multivariate-function-1
     (make-test-set unitary-function-2
                 ;; f(x) = sin(10 * x) + e^(x/2)
                 ;; df(x)/dx = 10 * cos(10 * x) * e^(x/2) - 1/2 * sin(10 * x) * e^(x/2)
                 ((f1 (lambda (x y) (+ (expt x 3) (expt y 2))))
                  (df1x (lambda (x y) (* 3 (expt x 2))))
                  (df1y (lambda (x y) (* 2 y)))
                  (df1x-aprox (lambda-derive-finite-diff f1  0.0001 x x y)))
                 ((test-within-bound? 0.01 (df1x-aprox 2 0 ) (df1x 2 0))
                  (test-within-bound? 0.01 (df1x-aprox 4  2) (df1x 4 100))
                  (test-within-bound? 0.01 (df1x-aprox -10 1) (df1x -10 99)))))

  (define (run-all)
    (unitary-function-1)
    (multivariate-function-1)))

(eval-when (load eval visit)
  (import (test finite-diff))
  (run-all))
