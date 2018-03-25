;
; Unit test framework for scheme
; Copyright (c) 2017, Peter Kofler, http://www.code-cop.org/
; BSD licensed.
;

; SchemeUnit from http://c2.com/cgi/wiki?SchemeUnit

(define (fail msg)
    (error (string-append "AssertionError: " msg)))

(define (check msg condition)
    (if (not condition) (fail msg)))

(define (assert msg condition)
    (lambda () (check msg condition)))

; extensions

(define (assert-generic-equal to-string eq-op expected actual)
    (assert
        (string-append "expected:<" (to-string expected) "> but was:<" (to-string actual) ">")
        (eq-op expected actual)))

(define (assert= expected actual)
    (assert-generic-equal number->string = expected actual))

(define (assert-string= expected actual)
    (assert-generic-equal values string=? expected actual))

(define (asserteq msg a b)
    (assert msg (> 0.0001 (abs (- a b)))))

; TODO values?

(define (list-equals-for eq-op)
    (define (list-equals? list1 list2)
        (cond
            ((null? list1)
                (null? list2))
            ((not (= (length list1) (length list2)))
                #f)
            ((not (eq-op (car list1) (car list2)))
                #f)
            (else
                (list-equals? (cdr list1) (cdr list2)))
        )
    )
    list-equals?
)

(define (assert-list= eq-op msg expected actual)
    (assert
        (string-append msg " lists not equal")
        ((list-equals-for eq-op) expected actual))
)

(define (assert-true actual)
    (assert "expected:<true> but was:<false>" actual))

(define (assert-false actual)
    (assert "expected:<false> but was:<true>" (not actual)))

(define (assert-raise expected-ex body)
    (define (error-exception->string ex)
        (cond ((symbol? ex) (symbol->string ex))
              ((string? ex) ex)
              ((error-exception? ex) (error-exception-message ex))
              (else (error "Argument not symbol or string or exception -- ASSERT-RAISE" ex))))
    (let ((expected-message (error-exception->string expected-ex)))
        (lambda ()
            (with-exception-catcher
                (lambda (ex)
                    (let ((actual-message (error-exception->string ex)))
                     (check
                        (string-append "Should raise " expected-message " but got " actual-message)
                        (string=? expected-message actual-message))))
                (lambda () (fail (body)))))))

(define (test-case name assertion)
    (display name)
    (newline)
    (assertion)
    (display "OK")
    (newline)
)

(define (ignored-test-case name assertion)
    (display name)
    (newline)
    (display "IGNORED")
    (newline)
)
