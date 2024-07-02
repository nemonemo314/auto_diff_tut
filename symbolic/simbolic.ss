(define-syntax deriv
  (syntax-rules (x - + * expt log / exp sin cos tan)
    [(_ (+ a b)) `(+ ,(deriv a) ,(deriv b))]
    [(_ (- a b)) `(- ,(deriv a) ,(deriv b))]
    [(_ (* a b)) `(+ (* ,(deriv a) b) (* a ,(deriv b)))]
    [(_ (/ a b)) `(/ (- (* ,(deriv a) b) (* a ,(deriv b))) (expt b 2 ))]
    [(_ (exp a)) `(* (expt a) ,(deriv a))]
    [(_ (expt a b)) `(+ (* b (* (expt a ,(- b 1)) ,(deriv a)))
                        (* (expt a b) (* (log a) ,(deriv b))))]
    [(_ (log a)) `(/ ,(deriv a) a)]
    [(_ (sin a)) `(* (cos a) ,(deriv a))]
    [(_ (cos a)) `(* (* -1 (sin a)) ,(deriv a))]
    [(_ (tan a)) `(* (/ 1 (expt (cos a) 2)) ,(deriv a))]
    [(_ x) 1]
    [(_ k) 0]))

(define-syntax derive
  (syntax-rules (with)
    [(_ expr with var)
     (begin (define-syntax deriv
        (syntax-rules (var - + * expt log / exp sin cos tan)
          [(_ (+ a b)) `(+ ,(deriv a) ,(deriv b))]
          [(_ (- a b)) `(- ,(deriv a) ,(deriv b))]
          [(_ (* a b)) `(+ (* ,(deriv a) b) (* a ,(deriv b)))]
          [(_ (/ a b)) `(/ (- (* ,(deriv a) b) (* a ,(deriv b))) (expt b 2 ))]
          [(_ (exp a)) `(* (expt a) ,(deriv a))]
          [(_ (expt a b)) `(+ (* b (* (expt a ,(- b 1)) ,(deriv a)))
                              (* (expt a b) (* (log a) ,(deriv b))))]
          [(_ (log a)) `(/ ,(deriv a) a)]
          [(_ (sin a)) `(* (cos a) ,(deriv a))]
          [(_ (cos a)) `(* (* -1 (sin a)) ,(deriv a))]
          [(_ (tan a)) `(* (/ 1 (expt (cos a) 2)) ,(deriv a))]
          [(_ var) 1]
          [(_ k) 0]))  (deriv expr) )]))


