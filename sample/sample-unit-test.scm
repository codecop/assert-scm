(include "../assert-r5rs.scm")

(test-case "1 + 1 = 2"
    (assert= 2 (+ 1 1)))
