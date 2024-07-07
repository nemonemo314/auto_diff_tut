(library (symbolic-tests)
  (export run-tests)
  (import (rnrs)
          (symbolic))

  (define (run-tests)
    (test-addition)
)

  (define (test-addition)

    (assert (equal? (syntax->datum (derive-symbolic x  (* x x))) '(* 2 x))))
)


