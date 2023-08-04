(include "prime-factors.scm")
(include "../assert-r5rs.scm")

(test-case "one"
    (assert-null (prime-factors 1)))

(test-case "two"
    (assert-number-list= (list 2) (prime-factors 2)))

(test-case "three"
    (assert-number-list= (list 3) (prime-factors 3)))

(test-case "four"
    (assert-number-list= (list 2 2) (prime-factors 4)))

(test-case "eight"
    (assert-number-list= (list 2 2 2) (prime-factors 8)))

(test-case "nine"
    (assert-number-list= (list 3 3) (prime-factors 9)))

(test-case "max"
    (assert-number-list= (list 2147483647) (prime-factors 2147483647)))
