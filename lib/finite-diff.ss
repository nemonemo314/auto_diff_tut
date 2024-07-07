(library (lib finite-diff)
  (export lambda-derive-finite-diff)
  (import (chezscheme))

  (meta define list-index
        (lambda ( lst proc)
          (let loop ([acc lst]
                     [i 0])
            (cond [(null? acc) #f]
                  [(proc (car acc)) i]
                  [else (loop (cdr acc) (+ 1 i))]))))

  (define-syntax lambda-derive-finite-diff
  (lambda (context)
    (syntax-case context ()
      [(_ f step var  v v* ...)
       (let* ((arg-list #'(v v* ...))
              (index (list-index arg-list (lambda (x) (free-identifier=? #'var x))))
              (stepped-args  (list-copy arg-list)))
         (if index
             (begin
               (set-car! (list-tail stepped-args index) #`(+ step #,(list-ref arg-list index)))
               #`(lambda (v v* ...) (/ (- (f #,@stepped-args) (f v v* ...)) step)))
             (syntax-error arg-list
                           (format #f
                                   "Variable ~a not found in the list of variables"
                                   (syntax->datum #'var)))))])))
)

