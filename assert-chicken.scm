;;;
;;; assert-scm - Minimalist xUnit test framework for Scheme R5RS, SRFI-12 extension (e.g. Chicken 4).
;;; https://github.com/codecop/assert-scm
;;; Copyright (c) 2015, Peter Kofler, BSD 3-Clause License
;;;
(include "assert-r5rs.scm")

(define (-error->string ex)
    (cond ((null? ex)                     "")
          ((string? ex)                   ex)
          ((symbol? ex)                   (symbol->string ex))
          ((list? ex)                     (string-append " (" (apply string-append (map -error->string ex)) ")"))
          ;; SRFI-12/Chicken specific code
          ((condition? ex)                (string-append (-error->string ((condition-property-accessor 'exn 'message) ex))
                                                         (-error->string ((condition-property-accessor 'exn 'arguments) ex))))
          (else                           (pp ex)
                                          "<unknown exception type>")))

(define (-run-with-exception-handler handler body)
    ;; SRFI-12 specific code
    (let ((exn-message-comparison (list)))
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
