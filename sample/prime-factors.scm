(define (prime-factors n)
    (test-candidate n 2))

(define (test-candidate n candidate)
    (cond
        ((= n 1) (list))
        ((too-large? n candidate) (prime n))
        ((divides? n candidate) (keep-candidate n candidate))
        (else (next-candidate n candidate))))

(define (too-large? n candidate)
    (> candidate (sqrt n)))

(define (prime n)
    (list n))

(define (divides? n candidate)
    (= (modulo n candidate) 0))

(define (keep-candidate n candidate)
    (append
        (prime candidate)
        (test-candidate (/ n candidate) candidate)))

(define (next-candidate n candidate)
    (test-candidate n (+ candidate 1)))
