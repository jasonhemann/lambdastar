(load "lambdastar.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(printf "variant 1~n")
(test-check "simple lambda* application"  
  ((lambda* (x) 4) 3)
  4)

(test-check "multiple args"
  ((lambda* (x y z) 4) 3 4 5)
  4)

(test-check "not just numbers"
  ((lambda* (x y z) 5) 'a 'b 'c)
  5)

(test-check "arguments all together"  
  (let ((h (lambda* (u)
               (lambda* (v)
                   (lambda* (w)
                       (lambda* (x)
                           (lambda* (y)
                               (+ u v w x y))))))))
    (let ((h^ (h 1 2 3)))
      (h^ 4 5)))
  15)

(test-check "formals all together, actuals split"
  (let ((h (lambda* (u v w x y) (+ u v w x y))))
    (let ((h^ (h 1 2 3)))
      (h^ 4 5)))
  15)


(test-check "test 0"
  (let ((h (lambda* (u)
             (lambda* (v)
               (lambda* (w)
                 (lambda* (x)
                   (lambda* (y)
                     (+ u v w x y))))))))
      (let ((h^ (h 1 2 3)))
        (h^ 4 5)))
  15)

(test-check "test1"
  (let ((h (lambda* (u v w x y) (+ u v w x y))))
      (let ((h^ (h 1 2 3)))
        (h^ 4 5)))
  15)

(test-check "test2"
  (let ((h (lambda* (u v)
             (lambda* (w x y)
               (+ u v w x y)) )))
      (let ((h^ (h 1 2 3)))
        (h^ 4 5)))
  15)

(test-check "test3"
  (((lambda* (a b c)
      (lambda* (d e)
        (+ a b c d e)))
      1 2 3)
     4 5)
  15)

(test-check "test3-other-args"
  (((lambda* (a b c)
      (lambda* (d e)
        (list a b c d e)))
      1 2)
     3 4 5)
  '(1 2 3 4 5))

(test-check "fewer parens"
  (((lambda* (a b c . ())
      (list a b c))
      1)
     2 3)
  '(1 2 3))

(define ctimes (lambda* (n y) (* n y)))

(define !
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (ctimes n (! (sub1 n)))))))

(define Y
  (lambda* (vo)
    ((lambda* (f) (f f))
     (lambda* (f) (vo (lambda* (x) ((f f) x)))))))

(define fact
  (Y (lambda* (! n)
       (cond
         ((zero? n) 1)
         (else (ctimes n (! (sub1 n))))))))

(test-check "test !5"
  (! 5)
  120)

(test-check "fact-5 2"
  (fact 5)
  120)

(test-check "facts"
  (letrec ((ctimes (lambda* (n y) (* n y)))
           (! (lambda (n)
                (cond
                  ((zero? n) 1)
                  (else (ctimes n (! (sub1 n))))))))
    (! 5))
  120)

;; app* works fine so long as you pass the proper number of arguments 

(test-check "in a mixed up order"
  (((lambda* (a b c)
        (lambda (d e)
          (+ a b c d e)))
    1 2 3)
   4 5)
  15)

(test-check "args arguments last"
  (((lambda* (a b c)
        (lambda args
          (+ a b c (car args) (cadr args))))
    1 2 3)
   4 5)
  15)

(test-check "a.d arguments first"
  (((lambda (a . d)
      (lambda* (e f g)
               `(,a ,e ,f ,g . ,d)))
    'u 'v 'w)
   'x 'y 'z)  
  '(u x y z v w))


(test-check "currying with regular lambda args mixed numbers of arguments"
  ((((lambda* (a b c)
         (lambda args
           (+ a b c)))
     1)
    4 5))
  10)

(test-check "with real lambda args on the outside, still plays nicely"
    (((lambda args
        (lambda* (a b c)
          (append args (list a b c))))      
      1 2 3)
     4 5 6)
    '(1 2 3 4 5 6))

(test-check "plays nicely with real lambda ()"  
  ((((lambda* (a b c)
         (lambda ()
           (+ a b c)))
     1)
    4 5))
  10)  

(printf "variant 2~n")
(test-check "easy test of nice play with regular lambda"
  ((lambda* (a b)
    (lambda (d e)
      (list a b d e)))
    1 2 3 4)
  '(1 2 3 4))

(test-check "plays nicely with regular lambdas"
  (((lambda* (a b)
        (lambda (d e)
          (list a b d e)))
    1)
   3 4 5)
  '(1 3 4 5))

(test-check "mixed up order, listing the arguments"
  (((lambda* (a b c)
        (lambda (d e)
          (list a b c d e)))
    1 2)
   3 4 5)
  '(1 2 3 4 5))

(test-check "a.d arguments last"
  ((lambda* (a b c)
            (lambda (e . f)
              `(,a ,b ,c ,e . ,f)))
   'u 'v 'w 'x 'y 'z)
  '(u v w x y z))

(printf "variant 3~n")
(test-check "fine with no arguments"
  (((lambda* (x) x)) 4)
  4)

(test-check "first w/some, then w/o, ..."
  (((((lambda* (x y) (+ x y))) 3)) 4)
  7)

(printf "variant 4~n")
(test-check "with args"
  ((lambda* (x . rest) (cons x rest)) 5)
  '(5))

(test-check "list of 5 6 7 8"
  (((lambda* (b c . rest) (cons* b c rest)) 5) 6 7 8)
  '(5 6 7 8))

(test-check "lambda* x1 xn . d works"
  ((lambda* (x y z a . b) (+ x y z)) 1 2 3 4 5 6)
  6)

(test-check "works even with list of no arguments"
  ((lambda* (x y z a . b) (+ x y z)) 1 2 3 4)
  6)

(test-check "dot d works as expected, curried"
  ((lambda* (a b . d) 5) 2 3)
  5)

(test-check "test3^^ without lambda"
  (((lambda* (a b c . args)
       (list a b c))
     1)
    2 3)
  '(1 2 3))

(test-check "With more"
  (((lambda* (a b c . d)
       (list a b c))
      1)
     2 3)
  '(1 2 3))

(printf "variant 5~n")
(test-check "lambda* args does its thing"
  ((lambda* args 4) 3 4 5)
  4)

(test-check "working with args"
  ((lambda* args args) 6 7 8)
  '(6 7 8))

(test-check "args and x . args also work nicely"
  (((lambda* args (lambda* (a . args) 4))) 3 4)
  4)

(test-check "test3^"
  (((lambda* (a b c)
      (lambda* args
        (+ a b c (car args) (cadr args))))
      1 2 3)
     4 5)
  15)

(test-check "test3^^"
  ((((lambda* (a b c)
       (lambda* args
         (list a b c)))
     1)
    2 3))
  '(1 2 3))

(test-check "test4"
  (((lambda* args
      (lambda* (a b c)
        (append args (list a b c))))      
      1 2 3)
     4 5 6)
  '(1 2 3 4 5 6))

(test-check "blackjack?"
  ((lambda* () 21))
  21)

(test-check "testing nil and x . args"
  (((lambda* () (lambda* (a . args) 4))) 3 4)
  4)

(test-check "test3^^^"
  ((((lambda* (a b c)
       (lambda* ()
         (list a b c)))
      1)
     2 3))
  '(1 2 3))

(test-check "testing nil and x . args"
  (((lambda* () (lambda* (a . args) 4))) 3 4)
  4)

(test-check "a test of nil and args"
  (((lambda* () (lambda* args 4))) 3 4)
  4)

(printf "variant 6~n")
(test-check "yes, mighty odd if you're looking at it for the first time"
  (((((lambda* (v) 
        (lambda* (w x) 
          (lambda* () 
            (lambda* (y . z) 
              (append (list v w x y) z)))))
      1 2 3)))
   4 5 6 7)
  '(1 2 3 4 5 6 7))

(test-check "w/advanced thunk behavior"
  ((lambda* ()
     (lambda* (a b c)
       (list a b c)))
   1 2 3)
  '(1 2 3))

(test-check "the odder side of the advanced thunking behavior"
  ((lambda* (a b c) 
     (lambda* ()
       (lambda* (d e f)
         (list a b c d e f))))
   1 2 3 4 5 6)
  '(1 2 3 4 5 6))

(test-check "so, one could also argue against this behavior as well"
  ((lambda* () (lambda* (a b c) (list a b c))) 1 2 3)
  '(1 2 3))

(printf "variant 7~n")
(test-check "multiple bodies, just simple test"
  ((lambda* (a) a 5) 6)
  5)

;; Should loop infinitely if tail recursion is preserved
;; (letrec ((fn (lambda* (x) (begin (+ 2 3) (fn x))))) (fn 1))
