#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Question 1
(define (sequence low high stride)
  (cond [(or (> low high) (< stride 0)) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

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
           [get-ith (lambda (ys acc) (if (= acc ith)
                                         (car ys)
                                         (get-ith (cdr ys) (+ acc 1))))])
    (cond [(negative? n)(error "list-nth-mod: negative number")]
          [(null? xs)(error "list-nth-mode: empty-list")]
          [#t (get-ith xs 0)])))
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 0)) 0
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 1)) 1
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 2)) 2
;(print (list-nth-mod (list 0 1 2 3 4 5 6 7) 3)) 3



;; Question 4
(define (stream-for-n-steps s n)
  (letrec ([recurse (lambda (acc stream) (cond [(= acc n) null]
                                               [#t (cons (car stream) 
                                                         (recurse (+ acc 1) ((cdr stream))))]))])
    (recurse 0 (s))))
;(define ones (lambda () (cons 1 (lambda () (ones)))))
;(stream-for-n-steps ones 10)



;; Question 5
(define funny-number-stream
  (letrec ([divisible-by-5 (lambda (x) (cond [(= (modulo x 5) 0) (- x)]
                                             [#t x]))]
           [make-stream (lambda (y) (cons (divisible-by-5 y) (lambda () (make-stream (+ y 1)))))])
        (lambda () (make-stream 1))))
;(stream-for-n-steps funny-number-stream 10)



;; Question 6
(define dan-then-dog
  (letrec ([alternate (lambda (name) (cond [(string=? name "dog.jpg") (cons name (lambda () (alternate "dan.jpg")))]
                                           [(string=? name "dan.jpg") (cons name (lambda () (alternate "dog.jpg")))]))])
    (lambda () (alternate "dan.jpg"))))
;(stream-for-n-steps dan-then-dog 10)



;; Quetion 7
(define (stream-add-zero s)
  (letrec ([make-stream (lambda (stream)
                          (let ([res (stream)])
                            (cons (cons 0 (car res))
                                  (lambda () (make-stream (cdr res))))))])
    (lambda () (make-stream s))))
;(define ones (lambda () (cons 1 (lambda () (ones)))))
;(stream-for-n-steps (stream-add-zero ones) 10)
;(stream-for-n-steps (stream-add-zero funny-number-stream) 10)



;; Question 8
(define (cycle-lists xs ys)
  (letrec ([make-stream (lambda (n1 n2) (cons 
                          (cons (list-nth-mod xs (car n1))
                                (list-nth-mod ys (car n2)))
                          (lambda () (make-stream ((cdr n1))((cdr n2))))))]
           [gen-n (lambda (n) (cons n (lambda () (gen-n (+ n 1)))))])
    (lambda () (make-stream (gen-n 0) (gen-n 0)))))
;(stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3)



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
;(assoc 3 (list (list 1 2) (list 3 4)))
;(vector-assoc 3 (vector (list 1 2) (list 3 4)))
;(vector-assoc 3 (list (list 1 2) (list 3 4))) ;produce error
;(vector-assoc 3 (vector (list 3 4)) 2) ;produce error 



; Question 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [position 0]
           [update-position (lambda () (cond [(= (+ position 1) n) (set! position 0)]
                                             [#t (set! position (+ position 1))]))])
    (lambda (v) (letrec ([in-cache (vector-assoc v cache)])   ; check if result already in cache (using vector-assoc)
                  (cond [(not (equal? in-cache #f)) in-cache] ; return result if found in cache
                        [#t (letrec ([result (assoc v xs)])   ; else call assoc with v(value) and xs(list) and return the res
                              (begin (cond [(not (equal? result #f)) (vector-set! cache position result)])
                                     (update-position)
                                     result))])))))
;(define xs (list (list 1 2) (list 3 4) (list 5 6)))
;(define res (cached-assoc xs 3))
;(res 1)
;(res 3)
;(res 5)
;(res 1) ;; from cache
;(res 6)
;(res 3) ;; from cache




; Question 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([res1 (if (procedure? e1) (e1) e1)]
              [res2 (lambda () (cons e2 (lambda () (res2))))]
              [main (lambda (fn) (letrec ([fn-res (fn)])
                                   (cond
                                     [(>= (car fn-res) res1) #t]
                                     [#t (main (cdr fn-res))])))])
       (main res2))]))
;(define a 2)
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))



