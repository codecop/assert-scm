;;;
;;; assert-scm - Minimalist xUnit test framework for Scheme R5RS.
;;; https://github.com/codecop/assert-scm
;;; Copyright (c) 2015, Peter Kofler, BSD 3-Clause License
;;;
;;; Non R5RS used functions
;;; * error from SRFI 23, available in Gambit, Chicken.
;;;

;; @formatter:align_list_of_strings:False

(define (fail msg)
    (error (-make-assertion-error msg)))

(define (check msg condition)
    (if (not condition)
        (fail msg)
        -success-marker))

;; colours

(include "ansi.scm")

(define (-in-white s)
    (ansi-string-with-color ansi-white s))

(define (-in-green s)
    (ansi-string-with-color ansi-green s))

(define (-in-yellow s)
    (ansi-string-with-color ansi-yellow s))

(define (-in-red s)
    (ansi-string-with-color ansi-red s))

;; extensions

(define (make-check msg condition)
    "All high level assertions are lazy to be grouped in test cases."
    (lambda ()
        (check msg condition)))

(define (-make-assertion-error msg)
    (-in-red (string-append "AssertionError" ": " msg)))

(define -success-marker
    (list 'success))

(define (-make-string-message prefix to-string expected actual)
    (-make-message prefix
                   (to-string expected)
                   (to-string actual)))

(define (-make-message prefix expected actual)
    (string-append prefix "expected:<" expected "> but was:<" actual ">"))

(define (assert-equal to-string eq-op expected actual)
    (make-check (-make-string-message "" to-string expected actual)
                (eq-op expected actual)))

(define (assert= expected actual)
    (assert-equal number->string = expected actual))

(define (assert-symbol= expected actual)
    (assert-equal symbol->string eq? expected actual))

(define (assert-string= expected actual)
    (assert-equal values string=? expected actual))

(define (assert-char= expected actual)
    (assert-equal string char=? expected actual))

;; private or library function
(define (-interval-inside? center radius x)
    (<= (abs (- center x))
        radius))

;; private - library function
(define (-interval->string center radius)
    (string-append "["
                   (number->string (- center radius))
                   "-"
                   (number->string (+ center radius))
                   "]"))

(define (assert-inexact= expected actual delta)
    (make-check (-make-message "in range "
                               (-interval->string expected delta)
                               (number->string actual))
                (-interval-inside? expected delta actual)))

(define (-make-message-numbered i expected actual)
    (define (item i)
        (string-append (number->string i)
                       ". item "))
    (-make-message (item i)
                   expected
                   actual))

(define (-check-numbered= i to-string eq-op expected actual)
    (check (-make-message-numbered i
                                   (to-string expected)
                                   (to-string actual))
           (eq-op expected actual)))

(define (assert-list= to-string eq-op expected-list actual-list)
    (define (check-list-element i expected actual)
        (let* ((expected-l (length expected))
               (actual-l   (length actual))
               (no-more?   (< expected-l actual-l))
               (has-more?  (> expected-l actual-l))
               (both-null? (and (null? expected) (null? actual))))
            (cond (both-null? -success-marker)
                  (no-more?   (fail (-make-message-numbered (+ i expected-l)
                                                            "no more elements"
                                                            "more elements")))
                  (has-more?  (fail (-make-message-numbered (+ i actual-l)
                                                            "more elements"
                                                            "no more elements")))
                  (else       (check-element i expected actual)))))
    (define (check-element i expected actual)
        (let* ((expected-element (car expected))
               (actual-element   (car actual)))
            (append (-check-numbered= i to-string eq-op expected-element actual-element)
                    (check-list-element (+ i 1)
                                        (cdr expected)
                                        (cdr actual)))))
    (lambda ()
        (check-list-element 1 expected-list actual-list)))

(define (assert-list-deep= to-string eq-op expected-list actual-list)
    (define (check-list-element i expected actual)
        (let* ((expected-l (length expected))
               (actual-l   (length actual))
               (no-more?   (< expected-l actual-l))
               (has-more?  (> expected-l actual-l))
               (both-null? (and (null? expected) (null? actual))))
            (cond (both-null? -success-marker)
                  (no-more?   (fail (-make-message-numbered (+ i expected-l)
                                                            "no more elements"
                                                            "more elements")))
                  (has-more?  (fail (-make-message-numbered (+ i actual-l)
                                                            "more elements"
                                                            "no more elements")))
                  (else       (check-element i expected actual)))))
    (define (check-element i expected actual)
        (let* ((expected-element (car expected))
               (actual-element   (car actual))
               (sublist?         (pair? expected-element))
               (no-sublist?      (pair? actual-element))
               (both-pair?       (and sublist? no-sublist?)))
            (cond (both-pair?  (append ; dummy chaining
                                       (check-list-element (+ 1 (* i 10))
                                                           expected-element
                                                           actual-element)
                                       (check-list-element (+ i 1)
                                                           (cdr expected)
                                                           (cdr actual))))
                  (sublist?    (fail (-make-message-numbered i "a sublist" "no sublist")))
                  (no-sublist? (fail (-make-message-numbered i "no sublist" "a sublist")))
                  (else        (append ; dummy chaining
                                       (-check-numbered= i to-string eq-op expected-element actual-element)
                                       (check-list-element (+ i 1)
                                                           (cdr expected)
                                                           (cdr actual)))))))
    (lambda ()
        (check-list-element 1 expected-list actual-list)))

(define (assert-string-list= expected-list actual-list)
    (assert-list-deep= values string=? expected-list actual-list))

(define (assert-number-list= expected-list actual-list)
    (assert-list-deep= number->string = expected-list actual-list))

;; private or library function
(define (-boolean->string b)
    (if b "true" "false"))

(define (assert-true actual)
    (make-check (-make-string-message "" -boolean->string #t #f)
                actual))

(define (assert-predicate predicate value)
    (assert-true (predicate value)))

(define assert-is? assert-predicate)

(define (assert-not-predicate predicate value)
    (assert-false (predicate value)))

(define assert-not-is? assert-not-predicate)

(define (assert-false actual)
    (make-check (-make-string-message "" -boolean->string #f #t)
                (not actual)))

(define (assert-null actual)
    (make-check (-make-message "" "null" "not null")
                (null? actual)))

(define (assert-not-null actual)
    (make-check (-make-message "" "not null" "null")
                (not (null? actual))))

(define (assert-all . assertions)
    (lambda ()
        (let ((results (map -test-case-assert assertions)))
            (if (zero? (apply + results))
                0
                -success-marker))))

(define (-test-case-name name)
    (display (-in-white name))
    (display " "))

(define (-test-case-success)
    (display (-in-green " OK"))
    (newline))

(define (-test-case-assert assert)
    (let ((result (assert)))
        (display ".")
        (if (and (list? result)
                 (not (null? result))
                 (eq? (car result)
                      (car -success-marker)))
            1
            0)))

(define (test-case name . assertions)
    (-test-case-name name)
    (let* ((results (map -test-case-assert assertions))
           (count   (apply + results)))
        (if (zero? count)
            (-test-case-useless)
            (-test-case-success))))

(define (-test-case-useless)
    (display (-in-yellow " USELESS"))
    (newline))

(define (-test-case-ignored)
    (display (-in-yellow " IGNORED"))
    (newline))

(define (ignored-test-case name . assertions)
    (-test-case-name name)
    (-test-case-ignored))
