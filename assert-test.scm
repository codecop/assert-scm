(include "assert.scm")

(test-case "(fail) throws AssertionError with message"
    (assert-raise "AssertionError: message"
        (lambda () (fail "message"))))

(check "(check) should not fail on #t condition" #t)

(test-case "(check) fails on #f condition"
    (lambda ()
        (with-exception-catcher
            (lambda (ex)
                 (check
                    (string-append "Should raise AssertionError: should fail")
                    (string=? "AssertionError: should fail" (error-exception-message ex))))
            (lambda () (check "should fail" #f)))))


(test-case "assert equals number"
    (assert= 1 1))

(test-case "assert equals string"
    (assert-string= "abc" "abc"))

(assert-list= = "int" (list 1 2) (list 1 2))
(assert-list= string=? "string" (list "a") (list "a"))

(test-case "assert boolean #t"
    (assert-true #t))

(test-case "assert boolean #f"
    (assert-false #f))

(test-case "assert-raise"
    (assert-raise
        'a
        (lambda () (raise 'a))))

(ignored-test-case "ignored, else it would fail"
    (assert-true #f))
