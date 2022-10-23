#lang racket

(define (must-be-two a b . c)
  (begin
    (writeln "test")
    (writeln a)
    (writeln b)))

;(must-be-two 1)     ; one short (will cause error)
(must-be-two 1 2)    ; exact
(must-be-two 1 2 3) ; one extra (will cause error)

(vector-length (vector 1 2 3))

(define a (vector 1 2 3))

(print (vector-ref a 0))
(= (vector-ref a 0) 1)


(writeln "")
;; Optional params
(define (testfunc a . b)
  (if (null? b)
    (writeln a)
    (writeln b)))


(testfunc 1); 1
(testfunc 1 2); '(2)
