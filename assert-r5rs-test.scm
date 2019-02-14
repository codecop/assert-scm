(include "assert-r5rs.scm")

(test-case "(check) should not fail on #t condition"
    (lambda ()
        (check "should not fail" #t)))

(test-case "(assert=) equals number"
    (assert= 1 1))

(test-case "(assert-string=) equals string"
    (assert-string= "abc" "abc"))

(test-case "(assert-inexact=) equals number"
    (assert-inexact= 1. 1.1 0.11))

(test-case "(assert-list=)"
    (assert-list= number->string
                  =
                  (list 1 2)
                  (list 1 2)))

(test-case "(assert-list=) recursive"
    (assert-list= number->string
                  =
                  (list 1 (list 2 3))
                  (list 1 (list 2 3))))

(test-case "(assert-list=) with strings"
    (assert-list= values
                  string=?
                  (list "a")
                  (list "a"))
    (assert-string-list= (list "a")
                         (list "a")))

(test-case "(assert-true)"
    (assert-true #t))

(test-case "(assert-false)"
    (assert-false #f))

(test-case "(assert-null)"
    (assert-null '()))

(test-case "(assert-not-null)"
    (assert-not-null (list 1)))

(test-case "(assert-all) allows several assertions"
    (assert-all
        (assert-true #t)
        (assert-true #t)))

(test-case "(test-case) allows several assertions"
    (assert-true #t)
    (assert-true #t))

(ignored-test-case "(ignored-test-case) is ignored, else it would fail"
    (assert-true #f))

(test-case "(useless-test-case)"
    (lambda () #f))
