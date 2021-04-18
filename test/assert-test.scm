(include "../assert.scm")

(define (test-failure name expected-message body)
    (test-case name
        (assert-raise (-make-assertion-error expected-message)
                      body)))

(test-failure "(fail) throws AssertionError with message"
    "message"
    (lambda ()
        (fail "message")))

(test-case "(check) should not fail on #t condition"
    (lambda ()
        (check "should not fail" #t)))

(test-failure "(check) fails on #f condition"
    "message"
    (lambda ()
        (check "message" #f)))

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
    (assert-list= number->string
                  =
                  (list 1 2)
                  (list 1 2)))

(test-failure "(assert-list=) fails on wrong element"
    "2. item expected:<3> but was:<2>"
    (assert-list= number->string
                  =
                  (list 1 3)
                  (list 1 2)))

(test-failure "(assert-list=) fails on short list"
    "3. item expected:<more elements> but was:<no more elements>"
    (assert-list= number->string
                  =
                  (list 1 2 3)
                  (list 1 2)))

(test-failure "(assert-list=) fails on long list"
    "2. item expected:<no more elements> but was:<more elements>"
    (assert-list= number->string
                  =
                  (list 1)
                  (list 1 2)))

(test-case "(assert-list=) recursive"
    (assert-list= number->string
                  =
                  (list 1 (list 2 3))
                  (list 1 (list 2 3))))

(test-failure "(assert-list=) recursive fails on wrong element type"
    "2. item expected:<a sublist> but was:<no sublist>"
    (assert-list= number->string
                  =
                  (list 1 (list 3))
                  (list 1 2)))

(test-failure "(assert-list=) recursive fails on wrong element"
    "22. item expected:<3> but was:<4>"
    (assert-list= number->string
                  =
                  (list 1 (list 2 3))
                  (list 1 (list 2 4))))

(test-case "(assert-list=) with strings"
    (assert-list= values
                  string=?
                  (list "a")
                  (list "a"))
    (assert-string-list= (list "a")
                         (list "a")))

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

(test-case "(assert-raise) on raise symbol"
    (assert-raise 'a (lambda ()
                         (raise 'a))))

(test-case "(assert-raise) on raise string"
    (assert-raise "a" (lambda ()
                          (raise "a"))))

(test-case "(assert-raise) on abort symbol"
    (assert-raise 'a (lambda ()
                         (abort 'a))))

(test-case "(assert-raise) on abort string"
    (assert-raise "a" (lambda ()
                          (abort "a"))))

(test-case "(assert-raise) on error symbol"
    (assert-raise 'a (lambda ()
                         (error 'a))))

(test-case "(assert-raise) on error string"
    (assert-raise "a" (lambda ()
                        (error "a"))))

(test-failure "(assert-raise) fails"
    "raise expected:<a> but was:<b>"
    (assert-raise 'a (lambda ()
                        (raise 'b))))

(test-failure "(assert-raise) fails when no raise"
    "raise expected:<a> but was:<no raise in body>"
    (assert-raise 'a (lambda ()
                        (+ 1 1))))

(test-case "(assert-raise) on unbound global variable"
    (assert-raise 'unbound-global-variable (lambda ()
                        (unbound-global-variable))))

(test-case "(assert-raise) on type error"
    (assert-raise "expected number" (lambda ()
                                        (+ 1 "1"))))

(test-case "(assert-all) allows several assertions"
    (assert-all
        (assert-true #t)
        (assert-true #t)))

(test-failure "(assert-all) evals all assertions"
    "expected:<true> but was:<false>"
    (lambda ()
        (test-case "- inside assert-all"
            (assert-all
                (assert-true #t)
                (assert-true #f)))))

(test-case "(test-case) allows several assertions"
    (assert-true #t)
    (assert-true #t))

(test-failure "(test-case) evals first assertion"
    "expected:<true> but was:<false>"
    (lambda ()
        (test-case "- inside evals first"
            (assert-true #f)
            (assert-true #t))))

(test-failure "(test-case) evals second assertion"
    "expected:<true> but was:<false>"
    (lambda ()
        (test-case "- inside evals second"
            (assert-true #t)
            (assert-true #f)
            (assert-true #t))))

(test-failure "(test-case) evals third assertion"
    "expected:<true> but was:<false>"
    (lambda ()
        (test-case "- inside evals third"
            (assert-true #t)
            (assert-true #t)
            (assert-true #f))))

(ignored-test-case "(ignored-test-case) is ignored, else it would fail"
    (assert-true #f))

(ignored-test-case "(ignored-test-case) is not evaluated, else it would crash"
    (assert-true (unbound-global-variable)))

(test-case "(useless-test-case)"
    (lambda () #f))

(test-failure "bug (assert-string-list=) fails on extra empty list"
    "124. item expected:<no more elements> but was:<more elements>"
    (assert-string-list= '(("4840" ("PRINT" ("\" \"") ";")))
                         '(("4840" ("PRINT" ("\" \"") ";" ())))))

(test-failure "bug (assert-string-list=) fails on different element after sublist"
    "3. item expected:<3> but was:<x>"
    (assert-string-list= '("1" ("2") "3" )
                         '("1" ("2") "x" )))
