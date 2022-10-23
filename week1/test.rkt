#lang racket

(assoc 3.5
       (list (list 1 2) (list 3 4) (list 5 6))
       (lambda (a b) (begin
                       (writeln a)
                       (writeln b)
                       (< (abs (- a b)) -1 ))))


; Vector

(define v1 (vector 'A 'P 'P 'L 'E))
(define v2 (vector  1  2  3  4  5)) 

(print v1)
(print v2)

(vector? v1)
(not (vector? v1))


(define (test a)
  (print a))

(test 'yes)
(test 'yes 'no)


