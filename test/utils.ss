(library (test utils)
  (export within-bound?  test-assert test-equal? test-within-bound? make-test-set)
  (import (rnrs)
          (only (chezscheme) format set-car! set-cdr!))

  (define (within-bound? bound val ground-truth)
    (and (<= val (+ ground-truth bound))
         (>= val (- ground-truth bound))))

  (define-syntax make-test-set
    (syntax-rules ()
      [(make-test-case name (test test* ...))
       (lambda ()
         (letrec [(test-log  (format #f "-***- Starting test-set ~a -***-~%" 'name))
               (test-count (length (list test test* ...)))
               (tally (cons 0 0))]
           
           (set! test-log (string-append test-log (format #f "-***- ~a has ~a tests~% -***-" 'name test-count)))
           (for-each (lambda (c) (run-and-report-case test-log tally c)) (list test test* ...))
           (format #t "~a" test-log)))]
      
      [(make-test-case name  ((init expr) ...) (test test* ...))
       (lambda ()
         (letrec [(test-log  (format #f "-***- Starting test-set: ~a -***-~%" 'name))
               (test-count (length (list test test* ...)))
               (tally (cons 0 0))
               (init expr) ...]
           (set! test-log (string-append test-log (format #f "~a has ~a tests~%" 'name test-count)))
           (for-each (lambda (c) (run-and-report-case test-log tally c)) (list test test* ...))
           (set! test-log
             (string-append test-log
                            (format #f "-***- Test set ~a ended with ~a failures and ~a successes -***-~%"
                                    'name (cdr tally) (car tally))))
           (format #t "~a" test-log)))]))

  (define-syntax run-and-report-case
    (syntax-rules ()
      [(_  log tally test-case)
       (call-with-values test-case
         (lambda (res msg)
           (if res
               (begin
                 (set-car! tally (+ 1 (car tally)))
                 (set! log (string-append log (format #f "Test ~a was a SUCCESS~%" (+ (car tally) (cdr tally))))))
               (begin
                 (set-cdr! tally (+ 1 (cdr tally)))
                 (set! log (string-append log (format #f "Test ~a was a FAILURE~%" (+ (car tally) (cdr tally)))))))
           (set! log (string-append log (format #f "~a~%" msg)))))]))

  (define-syntax test-assert
    (syntax-rules ()
      [(_ exp1)
       (lambda ()
         (if exp1
             (values #t (format #f "~a is true~%" 'exp1))
             (values #f (format #f "~a is not true~%" 'exp1))))]))

  (define-syntax test-equal?
    (syntax-rules ()
      [(_ exp1 exp2)
       (lambda ()
         (if (equal? exp1 exp2)
             (values #t
                     (format #f "(equal? ~a ~a)~% => #t ~%" 'exp1 'exp2))
             (values #f (format #f "(equal? ~a ~a) => #f~%~a is ~a while ~a is ~a~%"
                      'exp1 'exp2 'exp1 exp1 'exp2 exp2))))]))

  (define-syntax test-within-bound?
    (syntax-rules ()
      [(_ b v gd ) 
       (lambda ()
         (if (within-bound? b v gd)
             (values #t (format #f "~a => ~a ~%~a => ~a~%~a is within a +-~a bound from ~a~%" 'v v 'gd gd v b gd))
             (values #f (format #f "~a => ~a ~%~a => ~a~%~a is outside a +-~a bound from ~a~%" 'v v 'gd gd v b gd))))])))
