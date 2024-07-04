(library (symbolic)
  (export derive-symbolic
          lambda-derive-symbolic)
  (import (rnrs))

  ;; Macro que gera expressões não simplificadas para derivação
  (define-syntax derive-symbolic
    (lambda (context)
      (syntax-case context
          ( - + * expt log / exp sin cos tan)
        ;; operações basicas 
        [(_ var (+ a b)) #'(+ (derive-symbolic var a) (derive-symbolic var b))] 
        [(_ var (- a b)) #'(- (derive-symbolic var a) (derive-symbolic var b))]
        [(_ var (* a b)) #'(+ (* (derive-symbolic var a) b) (* a (derive-symbolic var b)))]
        [(_ var (/ a b)) #'(/ (- (* (derive-symbolic var a) b) (* a (derive-symbolic var b))) (expt b 2 ))]
        ;; e^f(x) = f(x)' * e^f(x) 
        [(_ var (exp a)) #'(* (expt a) (derive-symbolic var a))]
        ;; caso geral da regra da potencia
        ;; polinomial: x^n' = n * x ^ (n-1) * x'
        ;; constante: k ^ f(X) = ln(k) * k ^ f(x) * f(x)'
        [(_ var (expt a b))
         (cond
          [(and (number? (syntax->datum #'b)) (free-identifier=? #'var #'a)) ; caso polynomio
           #'(* b (* (expt a (- b 1)) (derive-symbolic var a)))]
          [(and (number? (syntax->datum #'a)) (free-identifier=? #'var #'b)) ; caso k^x
           #'(* (expt a b) (* (log a) (derive-symbolic var b)))]
          [else #'(+ (* b (* (expt a (- b 1)) (derive-symbolic var a))) ; caso generico
                     (* (expt a b) (* (log a) (derive-symbolic var b))))])]
        
        ;; ln(x)' = x'/x
        [(_ var (log a)) #'(/ (derive-symbolic var a) a)]
        
        ;; trigonometricas
        ;; sin(x)' = cos x
        ;; cos(x)' = -sin(x)
        ;; tg(x)'  = 1/sec(x)^2 
        [(_ var (sin a)) #'(* (cos a) (derive-symbolic var a))] 
        [(_ var (cos a)) #'(* (* -1 (sin a)) (derive-symbolic var a))] 
        [(_ var (tan a)) #'(* (/ 1 (expt (cos a) 2)) (derive-symbolic var a))]
        [(_ var v) (if (free-identifier=? #'var #'v) 1 0)])))

  ;; Macro que envelopa as expressões de derivação em um lambda
  ;; (lambda-derive-symbolic expr var vars ...) expande  em  (lambda (var vars ...) (derive-symbolic var expr))
  ;; Permite utilizar a expressão simbolica para calcular valores numericos. É nescessario informar todas as variaveis.
  ;; A primeira variavel, var, é o sentido de derivação.
  (define-syntax lambda-derive-symbolic
    (syntax-rules ()
      [(_ expr var vars ...) (lambda (var vars ...) (derive-symbolic var expr))] )))

