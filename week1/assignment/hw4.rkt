#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Question 1
(define (sequence low high stride)
  (cond [(or (> low high) (< stride 0)) null]
        [(= (+ low stride) high) high]
        [#t (cons low (sequence 
                       (+ low stride) 
                       high 
                       stride))]))

;(sequence 3 11 2) ; `(3 5 7 9 11)
;(sequence 3 8 3)  ; `(3 6)
;(sequence 3 2 1)  ; `()



;; Question 2
(define (string-append-map xs suffix)
  (map
   (lambda (s) (string-append s suffix))
   xs))

;(string-append-map (list "market" "paint" "list") "ing")



;; Question 3

(define (list-nth-mod xs n)
  (letrec ([ith (remainder n (length xs))]
           [get-ith (lambda (ys acc) (if (= acc ith) (car ys) (get-ith (cdr ys) (+ acc 1))))])
    (cond [(negative? n)(error "list-nth-mod: negative number")]
          [(null? xs)(error "list-nth-mode: empty-list")]
          [#t (get-ith xs 0)])))

;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 0)) 0
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 1)) 1
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 2)) 2
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 3)) 3


;; Question 4
(define (stream-for-n-steps s0 n)
  (letrec ([recurs (lambda (s acc)
                     (cond [(= acc n) null]
                           [#t (let ([res ((cdr s))])
                           (cons (car s) (recurs res (+ acc 1))))]))])
    (recurs s0 0)))



;; Question 5
(define funny-number-stream
  (letrec ([divisible-by-5 (lambda (x) (cond [(= (modulo x 5) 0) (- x)]
                                             [#t x]))]
           [make-stream (lambda (y) (cons (divisible-by-5 y) (lambda () (make-stream (+ y 1)))))])
        (make-stream 1)))

;(define a funny-number-stream)
;(define b ((cdr a)))
;(define c ((cdr b)))
;(define d ((cdr c)))
;(define e ((cdr d)))
;(define f ((cdr e)))
;(define g ((cdr f)))


;; Question 6
(define dan-then-dog
  (letrec ([alternate (lambda (name) (cond [(string=? name "dog.jpg") (cons name (lambda () (alternate "dan.jpg")))]
                                           [(string=? name "dan.jpg") (cons name (lambda () (alternate "dog.jpg")))]))])
    (alternate "dan.jpg")))

;(define a dan-then-dog)
;(define b ((cdr a)))
;(define c ((cdr b)))
;(define d ((cdr c)))
;(define e ((cdr d)))



;; Quetion 7
(define (stream-add-zero s)
  (letrec ([make-stream (lambda (stream) 
                          (cons
                            (cons 0 (car stream))
                            (lambda () (make-stream ((cdr stream))))))])
    (make-stream s)))
  
;(define a (stream-add-zero dan-then-dog))
;(define b ((cdr a)))
;(define c ((cdr b)))
;(define d ((cdr c)))


;; Question 8
(define (cycle-lists xs ys)
  (letrec ([make-stream (lambda (n1 n2) (cons 
                          (cons (list-nth-mod xs (car n1))
                                (list-nth-mod ys (car n2)))
                          (lambda () (make-stream ((cdr n1))((cdr n2))))))]
           [gen-n (lambda (n) (cons n (lambda () (gen-n (+ n 1)))))])
    (make-stream (gen-n 0) (gen-n 0))))

;(define res (cycle-lists (list 1 2 3) (list "a" "b")))
;res
;((cdr res))
;((cdr ((cdr res))))



;; Question 9
(define (vector-assoc v vec . extra-params)
  (letrec ([check-match (lambda (x) (= (car x) v))]
           [recurse (lambda (n) 
                      (cond [(= (vector-length vec) n) #f]
                            [(pair? (vector-ref vec n)) (if (check-match (vector-ref vec n))
                                                      (vector-ref vec n)
                                                      (recurse (+ n 1)))]
                            [#t (recurse (+ n 1))]))])
          (cond [(not (vector? vec)) 
                             (error "vector-assoc: second arg not a vector.")]
                [(not (null? extra-params))
                             (error "vector-assoc: must pass only two args.")]
                [#t (recurse 0)])))


(assoc 3 (list (list 1 2) (list 3 4)))
(vector-assoc 3 (vector (list 1 2) (list 3 4)))
;(vector-assoc 3 (list (list 1 2) (list 3 4))) ;produce error
;(vector-assoc 3 (vector (list 3 4)) 2) ;produce error 


;; Question 10
(define (cached-assoc xs n)
  (letrec ([])
    (...)))



