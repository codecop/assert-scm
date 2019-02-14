;;;
;;; Unit test framework for Scheme R5RS, Chicken extension.
;;; Copyright (c) 2015, Peter Kofler, http://www.code-cop.org/
;;; BSD licensed.
;;;
(include "assert-r5rs.scm")

(define (-error->string ex)
    (cond ((symbol? ex)                   (symbol->string ex))
          ((string? ex)                   ex)
          ;; Chicken specific code
          ;; TODO Chicken Scheme exception types
          ;; ((type-exception? ex)           (string-append "expected " (-error->string (type-exception-type-id ex)))) ; type name
          ;; ((exn ex)           "xxx")
          ;; see https://wiki.call-cc.org/man/4/Exceptions#exception-handlers
          (else                  (pp ex)
                                 "<unknown exception type>")))

(define (-run-with-exception-handler handler body)
    ;; Chicken specific code
    (let ((exn-message-comparison '()))
        (handle-exceptions exn
            (set! exn-message-comparison (handler exn))
            (body))
        exn-message-comparison))

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
