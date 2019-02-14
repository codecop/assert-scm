;;;
;;; Unit test framework for Scheme R5RS, Gambit extension.
;;; Copyright (c) 2015, Peter Kofler, http://www.code-cop.org/
;;; BSD licensed.
;;;
(include "assert-r5rs.scm")

(define (-error->string ex)
    (cond ((symbol? ex)                   (symbol->string ex))
          ((string? ex)                   ex)
          ;; Gambit specific code
          ((error-exception? ex)          (-error->string (error-exception-message ex)))
          ((unbound-global-exception? ex) (-error->string (unbound-global-exception-variable ex))) ; variable name
          ((type-exception? ex)           (string-append "expected " (-error->string (type-exception-type-id ex)))) ; type name
          (else                           (pp ex)
                                          "<unknown exception type>")))

(define (-run-with-exception-handler handler body)
    ;; Gambit specific code
    (with-exception-catcher handler body))

(define (assert-raise expected-ex body)
    (define (ex-handler ex)
        (let ((expected-message (-error->string expected-ex))
              (actual-message   (-error->string ex)))
            (check (-make-string-message "raise " -error->string expected-ex ex)
                   (string=? expected-message actual-message))))
    (define (ex-body)
        (body)
        (error "no raise in body"))
    (lambda ()
        (-run-with-exception-handler ex-handler ex-body)))

(define-macro (ignored-test-case name . assertions)
    `(begin
        (-test-case-name ,name)
        (-test-case-ignored)))
