(define-syntax deriv
  (lambda (context)
    (syntax-case context
        ( - + * expt log / exp sin cos tan)
      [(_ var (+ a b)) #'(+ (deriv var a) (deriv var b))] 
      [(_ var (- a b)) #'(- (deriv var a) (deriv var b))]
      [(_ var (* a b)) #'(+ (* (deriv var a) b) (* a (deriv var b)))]
      [(_ var (/ a b)) #'(/ (- (* (deriv var a) b) (* a (deriv var b))) (expt b 2 ))]
      [(_ var (exp a)) #'(* (expt a) (deriv var a))]
      [(_ var (expt a b))
       (cond
        [(and (number? (syntax->datum #'b)) (free-identifier=? #'var #'a)) ; caso polynomio
         #'(* b (* (expt a (- b 1)) (deriv var a)))]
        [(and (number? (syntax->datum #'a)) (free-identifier=? #'var #'b)) ; caso k^x
         #'(* (expt a b) (* (log a) (deriv var b)))]
        [else #'(+ (* b (* (expt a (- b 1)) (deriv var a))) ; caso generico
                   (* (expt a b) (* (log a) (deriv var b))))])]
      [(_ var (log a)) #'(/ (deriv var a) a)]
      [(_ var (sin a)) #'(* (cos a) (deriv var a))]
      [(_ var (cos a)) #'(* (* -1 (sin a)) (deriv var a))]
      [(_ var (tan a)) #'(* (/ 1 (expt (cos a) 2)) (deriv var a))]
      [(_ var v) (if (free-identifier=? #'var #'v) 1 0)])))

(define-syntax lambda-derive
  (syntax-rules ()
    [(_ expr var vars ...) (lambda (var vars ...) (deriv var expr))] ))

