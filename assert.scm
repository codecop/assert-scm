;
; Unit test framework for scheme
; Copyright (c) 2017, Peter Kofler, http://www.code-cop.org/
; BSD licensed.
;

; SchemeUnit from http://c2.com/cgi/wiki?SchemeUnit

(define (fail msg)
    (error (string-append "AssertionError" ": " msg)))

(define (check msg condition)
    (if (not condition) (fail msg)))

(define (assert msg condition)
    (lambda () (check msg condition)))

; extensions

; private
(define (expected-but-actual prefix to-string expected actual)
    (string-append prefix "expected:<" (to-string expected) "> but was:<" (to-string actual) ">"))

; private
(define (assert-generic-equal to-string eq-op expected actual)
    (assert
        (expected-but-actual "" to-string expected actual)
        (eq-op expected actual)))

(define (assert= expected actual)
    (assert-generic-equal number->string = expected actual))

(define (assert-string= expected actual)
    (assert-generic-equal values string=? expected actual))

(define (assert-inexact= expected actual delta)
    (define (in-interval? center radius x)
        (<= (abs (- center x)) radius))
    (define (interval->string center radius)
        (string-append "[" (number->string (- center radius)) "-" (number->string (+ center radius)) "]"))
    (assert
        (expected-but-actual "in range " values (interval->string expected delta) (number->string actual))
        (in-interval? expected delta actual)))

(define (assert-list= eq-op expected actual)
    (define (list-equals? i list1 list2)
        (cond ((null? list1) (check "" (null? list2)))
              ((not (= (length list1) (length list2))) #f)
              ((not (eq-op (car list1) (car list2))) #f)
              (else (list-equals? (+ i 1) (cdr list1) (cdr list2)))
        )
    )
    (assert
        (string-append " lists not equal")
        (list-equals? 1 expected actual))
)

; private
(define (boolean->string b)
    (if b "true" "false"))

(define (assert-true actual)
    (assert (expected-but-actual "" boolean->string #t #f) actual))

(define (assert-false actual)
    (assert (expected-but-actual "" boolean->string #f #t) (not actual)))

(define (assert-raise expected-ex body)
    (define (error-exception->string ex)
        (cond ((symbol? ex) (symbol->string ex))
              ((string? ex) ex)
              ((error-exception? ex) (error-exception->string (error-exception-message ex)))
              (else "")))
              ; (error "Argument not symbol or string or exception -- ASSERT-RAISE" ex)
    (lambda ()
        (with-exception-catcher
            (lambda (ex)
                (let ((expected-message (error-exception->string expected-ex))
                      (actual-message (error-exception->string ex)))
                    (check
                        (expected-but-actual "raise " error-exception->string expected-ex ex)
                        (string=? expected-message actual-message))))
            (lambda () (fail (body))))))

(define (test-case name assertion)
    (display name)
    (newline)
    (assertion)
    (display "OK")
    (newline))

(define (ignored-test-case name assertion)
    (display name)
    (newline)
    (display "IGNORED")
    (newline))
