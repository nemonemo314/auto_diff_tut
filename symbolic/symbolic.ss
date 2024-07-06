(library (symbolic)
  (export derive-symbolic
          lambda-derive-symbolic)
  (import (rnrs))

  ;; Macro que gera expressões não simplificadas para derivação
  (define-syntax derive-symbolic
    (lambda (context)
      (syntax-case context ( - + * expt log / exp sin cos tan)
        [(_ var (+ a b)) #'(+ (derive-symbolic var a) (derive-symbolic var b))] 
        [(_ var (- a b)) #'(- (derive-symbolic var a) (derive-symbolic var b))]
        [(_ var (* a b)) #'(+ (* (derive-symbolic var a) b) (* a (derive-symbolic var b)))]
        [(_ var (/ a b)) #'(/ (- (* (derive-symbolic var a) b) (* a (derive-symbolic var b))) (expt b 2 ))]
        [(_ var (exp a)) #'(* (expt a) (derive-symbolic var a))]
        [(_ var (expt a b))
         (cond
          [(and (number? (syntax->datum #'b)) (free-identifier=? #'var #'a)) ; caso polinômio
           #'(* b (* (expt a (- b 1)) (derive-symbolic var a)))]
          [(and (number? (syntax->datum #'a)) (free-identifier=? #'var #'b)) ; caso k^x
           #'(* (expt a b) (* (log a) (derive-symbolic var b)))]
          [else #'(+ (* b (* (expt a (- b 1)) (derive-symbolic var a))) ; caso genérico
                     (* (expt a b) (* (log a) (derive-symbolic var b))))])]
        [(_ var (log a)) #'(/ (derive-symbolic var a) a)]    
        [(_ var (sin a)) #'(* (cos a) (derive-symbolic var a))] 
        [(_ var (cos a)) #'(* (* -1 (sin a)) (derive-symbolic var a))] 
        [(_ var (tan a)) #'(* (/ 1 (expt (cos a) 2)) (derive-symbolic var a))]
        [(_ var v) (cond [(number? (syntax->datum #'v)) 0]
                         [(free-identifier=? #'var #'v) 1]
                         [else 0])])))

  (define-syntax lambda-derive-symbolic
    (syntax-rules ()
      [(_ expr var vars ...) (lambda (var vars ...) (derive-symbolic var expr))])))

