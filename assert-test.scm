(include "assert.scm")

(define (test-failure name expected-message body)
    (test-case name
        (assert-raise (string-append "AssertionError: " expected-message)
            body)))

(test-failure "(fail) throws AssertionError with message"
    "message"
    (lambda () (fail "message")))

(test-case "(check) should not fail on #t condition"
    (lambda () (check "should not fail" #t)))

(test-failure "(check) fails on #f condition"
    "message"
    (lambda () (check "message" #f)))

(test-case "(assert=) equals number"
    (assert= 1 1))

(test-failure "(assert=) fails"
    "expected:<1> but was:<2>"
    (assert= 1 2))

(test-case "(assert-string=) equals string"
    (assert-string= "abc" "abc"))

(test-failure "(assert-string=) fails"
    "expected:<abc> but was:<123>"
    (assert-string= "abc" "123"))

(test-case "(assert-inexact=) equals number"
    (assert-inexact= 1. 1.1 0.11))

(test-failure "(assert-inexact=) fails"
    "in range expected:<[.99-1.01]> but was:<1.1>"
    (assert-inexact= 1. 1.1 0.01))

(test-case "(assert-list=)"
    (assert-list= number->string = (list 1 2) (list 1 2)))

(test-failure "(assert-list=) fails on wrong element"
    "2. item expected:<3> but was:<2>"
    (assert-list= number->string = (list 1 3) (list 1 2)))

(test-failure "(assert-list=) fails on short list"
    "3. item expected:<more elements> but was:<no more elements>"
    (assert-list= number->string = (list 1 2 3) (list 1 2)))

(test-failure "(assert-list=) fails on long list"
    "2. item expected:<no more elements> but was:<more elements>"
    (assert-list= number->string = (list 1) (list 1 2)))

(test-case "(assert-list=) with strings"
    (assert-list= values string=? (list "a") (list "a")))

(test-case "(assert-true)"
    (assert-true #t))

(test-failure "(assert-true) fails"
    "expected:<true> but was:<false>"
    (assert-true #f))

(test-case "(assert-false)"
    (assert-false #f))

(test-failure "(assert-false) fails"
    "expected:<false> but was:<true>"
    (assert-false #t))

(test-case "(assert-null)"
    (assert-null '()))

(test-failure "(assert-null) fails"
    "expected:<null> but was:<not null>"
    (assert-null (list 1)))

(test-case "(assert-not-null)"
    (assert-not-null (list 1)))

(test-failure "(assert-not-null) fails"
    "expected:<not null> but was:<null>"
    (assert-not-null '()))

(test-case "(assert-raise) on raise"
    (assert-raise
        'a
        (lambda () (raise 'a))))

(test-case "(assert-raise) on raise string"
    (assert-raise
        "a"
        (lambda () (raise 'a))))

(test-case "(assert-raise) on error"
    (assert-raise
        'a
        (lambda () (error 'a))))

(test-case "(assert-raise) on error string"
    (assert-raise
        "a"
        (lambda () (error "a"))))

(test-failure "(assert-raise) fails"
    "raise expected:<a> but was:<b>"
    (assert-raise 'a (lambda () (raise 'b))))

(test-failure "(assert-raise) fails when no raise"
    "raise expected:<a> but was:<>"
    (assert-raise 'a (lambda () (+ 1 1))))

(ignored-test-case "(ignored-test-case) is ignored, else it would fail"
    (assert-true #f))
