
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence l h st)
  (if (> l h)
      null
      (cons l (sequence (+ l st) h st))))


(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (let ([i (remainder n (length xs))])
          (car (list-tail xs i)))]))


(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([p (s)])
    (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))


(define funny-number-stream
  (letrec ([helper (lambda (x) (if (= 0 (remainder x 5))
                               (cons (- x) (lambda () (helper (+ x 1))))
                               (cons x (lambda () (helper (+ x 1))))))])
    (lambda () (helper 1))))


(define dan-then-dog
  (letrec ([h (lambda (x) (if (= x 0)
                              (cons "dan.jpg" (lambda () (h (+ 1 x))))
                              (cons "dog.jpg" (lambda () (h (- 1 x))))))])
    (lambda () (h 0))))


(define (stream-add-zero s)
  (lambda ()
    (letrec ([p (s)])
      (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))  


(define (cycle-lists xs ys)
  (letrec ([xl (length xs)] [yl (length ys)]
              [h (lambda (n) (cons (cons (list-nth-mod xs (remainder n xl))
                                         (list-nth-mod ys (remainder n yl)))
                                   (lambda () (h (+ n 1)))))])
    (lambda () (h 0))))


(define (vector-assoc v vec)
  (letrec ([h
            (lambda (p) (if (= p (vector-length vec)) #f
                            (let ([val (vector-ref vec p)])
                             (if (pair? val)
                                 (if (equal? (car val) v)
                                     val
                                     (h (+ p 1)))
                                 (h (+ p 1))))))])
    (h 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cp -1]
           [h (lambda (v)
                (letrec ([c (vector-assoc v cache)])
                  (if (equal? #f c)
                      (begin (let ([resu (assoc v xs)])
                               (if (equal? #f resu) #f
                                 (begin
                                   (set! cp (+ 1 cp))
                                   (vector-set! cache (remainder cp n) resu)
                                   resu))))                             
                      (car c))))])
    h))

(define-syntax while-less
  (syntax-rules (do)
                [(while-less e1 do e2)
                 ((let ([r1 e1]
                        [r2 e2])
                    (letrec ([h (lambda (i)
                                  (if (or (> i r1) (= i r1)) (lambda () #t)
                                      (h e2)))])
                      (h r2))))]))
                      
        
  
