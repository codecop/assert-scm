;
; Unit test framework for scheme
; Copyright (c) 2017, Peter Kofler, http://www.code-cop.org/
; BSD licensed.
;

; SchemeUnit from http://c2.com/cgi/wiki?SchemeUnit

(define (fail msg)
    (error (string-append "AssertionError" ": " msg)))

(define (check msg condition)
    (if (not condition) (fail msg) #t))

(define (assert msg condition)
    (lambda () (check msg condition)))

; extensions

; private
(define (make-string-message prefix to-string expected actual)
    (make-message prefix (to-string expected) (to-string actual)))

; private
(define (make-message prefix expected actual)
    (string-append prefix "expected:<" expected "> but was:<" actual ">"))

(define (assert-equal to-string eq-op expected actual)
    (assert
        (make-string-message "" to-string expected actual)
        (eq-op expected actual)))

(define (assert= expected actual)
    (assert-equal number->string = expected actual))

(define (assert-string= expected actual)
    (assert-equal values string=? expected actual))

(define (assert-inexact= expected actual delta)
    (define (in-interval? center radius x)
        (<= (abs (- center x)) radius))
    (define (interval->string center radius)
        (string-append "[" (number->string (- center radius)) "-" (number->string (+ center radius)) "]"))
    (assert
        (make-message "in range " (interval->string expected delta) (number->string actual))
        (in-interval? expected delta actual)))

(define (assert-list= to-string eq-op expected-list actual-list)
    (define (item i)
        (string-append (number->string i) ". item "))
    (define (check-numbered i expected actual)
        (check
            (make-message (item i) (to-string expected) (to-string actual))
            (eq-op expected actual)))

    (define (check-list-element i expected actual)
        (cond ((and (null? expected) (null? actual)) #t)
              ((null? expected) (fail (make-message (item i) "no more elements" "more elements")))
              ((null? actual)   (fail (make-message (item i) "more elements" "no more elements")))
              (else (cons ; dummy chaining
                        (check-numbered i (car expected) (car actual))
                        (check-list-element (+ i 1) (cdr expected) (cdr actual))))))
    (lambda ()
        (check-list-element 1 expected-list actual-list)))

; private
(define (boolean->string b)
    (if b "true" "false"))

(define (assert-true actual)
    (assert (make-string-message "" boolean->string #t #f) actual))

(define (assert-false actual)
    (assert (make-string-message "" boolean->string #f #t) (not actual)))

(define (assert-null actual)
    (assert (make-message "" "null" "not null") (null? actual)))

(define (assert-not-null actual)
    (assert (make-message "" "not null" "null") (not (null? actual))))

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
                        (make-string-message "raise " error-exception->string expected-ex ex)
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
