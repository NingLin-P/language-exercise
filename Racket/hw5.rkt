;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist list)
  (cond [(null? list) (aunit)]
        [#t (apair (car list) (racketlist->mupllist (cdr list)))]))

(define (mupllist->racketlist list)
  (cond [(apair? list)
         (let ([v1 (apair-e1 list)]
               [v2 (apair-e2 list)])
           (cond [(aunit? v2) (cons v1 null)]
                 [#t (cons v1 (mupllist->racketlist v2))]))]
        [#t (error (format "bad MUPL expression: ~v" list))]))

         
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str)  (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        
        [(int? e) e]
        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        
        [(aunit? e) e]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        
        [(ifgreater? e)
         
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (cond [(> (int-num v1) (int-num v2))
                      (eval-under-env (ifgreater-e3 e) env)]
                     [#t (eval-under-env (ifgreater-e4 e) env)])
               (error "e1 or e2 No a int expression in MUPL")))]
        
        [(mlet? e) ;; var e body
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        
        
        [(call? e)  ;;call (funexp actual)  closure (env fun)  fun  (nameopt formal body)
         (let ([clos (eval-under-env (call-funexp e) env)]
               [para-val (eval-under-env (call-actual e) env)])
           (if (fun? clos) (eval-under-env (call clos para-val) env)
           (cond [(closure? clos)
                  (let ([clos-env (closure-env clos)]
                        [clos-fun (closure-fun clos)])
                    (cond [(fun? clos-fun) 
                           (letrec ([name (fun-nameopt clos-fun)]
                                    [formal (fun-formal clos-fun)]
                                    [body (fun-body clos-fun)]
                                    [para-env (cons (cons formal para-val) clos-env)])
                             (cond [name (eval-under-env body (cons (cons name clos-fun) para-env))]
                                   [#t (eval-under-env body para-env)]))]
                          [#t (error (format "non-fun in closure: ~v" clos-fun))]))]
                          
                 [#t (begin (print (call-funexp e)) (error (format "apply non-closure to call: ~v" clos)))]
                 )))]
                                   
                             
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (eval-under-env (apair-e1 p) env)
               (error (format "not a apair apply to snt: ~v" p))))]
                           
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (eval-under-env (apair-e2 p) env)
               (error (format "not a apair apply to snt: ~v" p))))]
        [(isaunit? e)
         (let ([aun (eval-under-env (isaunit-e e) env)])
           (if (aunit? aun)
               (int 1)
               (int 0)))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))
  
(define (mlet* lstlst e2)
  (cond [(null? (cdr lstlst)) (mlet (car (car lstlst)) (cdr (car lstlst)) e2)]
        [#t (mlet* (cdr lstlst) (mlet (car (car lstlst)) (cdr (car lstlst)) e2))]))
                        
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "e1" e1) (cons "e2" e2))
         (ifgreater (var "e1") (var "e2") e4 (ifgreater (var "e2") (var "e1") e4 e3))))
  

;; Problem 4
;; (struct fun  (nameopt formal body)
;; (struct call (funexp actual)
;; (struct closure (env fun)
;; mlet (var e body)

(define mupl-map
  (fun #f "func"
       (fun "recfun" "list"
                      (ifaunit (snd (var "list"))
                               (apair (call (var "func") (fst (var "list"))) (aunit))
                               (apair (call (var "func") (fst (var "list"))) (call (var "recfun") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "inte"
             (call (var "map") (fun #f "x" (add (var "x") (var "inte")))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
