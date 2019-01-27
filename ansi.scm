;;;
;;; Ansi terminal colours.
;;;

(define ansi-black "30")
(define ansi-red "31")
(define ansi-green "32")
(define ansi-yellow "33")
(define ansi-blue "34")
(define ansi-magenta "35")
(define ansi-cyan "36")
(define ansi-white "37")
(define ansi-off "0")

(define (-ansi-control-char code)
    (string-append "\033" "[" code "m"))

(define (ansi-string-with-color colour s)
    (string-append (-ansi-control-char colour)
                   s
                   (-ansi-control-char ansi-off)))

(define (ansi-error s)
    (ansi-string-with-color ansi-red s))

(define (ansi-warning s)
    (ansi-string-with-color ansi-yellow s))

;; (display (ansi-error "abc"))
;; (display (ansi-warning "abc"))
