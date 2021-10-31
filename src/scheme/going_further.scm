;; -*- coding: utf-8; -*-
(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (and e2 e3 ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1])
       (if t t (or e2 e3 ...)))]))

#|+bad-and|#
(define-syntax and ; incorrect!
  (syntax-rules ()
    [(_) #t]
    [(_ e1 e2 ...)
     (if e1 (and e2 ...) #f)]))
#|-bad-and|#

(if a (and b c) #f)

(if a (if b (and c) #f) #f)

(if a (if b c #f) #f)

(and (not (= x 0)) (/ 1 x))

(if (not (= x 0)) (and (/ 1 x)) #f) →
(if (not (= x 0)) (if (/ 1 x) (and) #f) #f) →
(if (not (= x 0)) (if (/ 1 x) #t #f) #f)

#|+bad-or|#
(define-syntax or ; incorrect!
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t t (or e2 ...)))]))
#|-bad-or|#

(let ([sum (lambda (sum ls)
             (if (null? ls)
                 0
                 (+ (car ls) (sum sum (cdr ls)))))])
  (sum sum '(1 2 3 4 5))) → 15

(letrec ([sum (lambda (ls)
                (if (null? ls)
                    0
                    (+ (car ls) (sum (cdr ls)))))])
  (sum '(1 2 3 4 5))) → 15

(letrec ([even?
          (lambda (x)
            (or (= x 0)
                (odd? (- x 1))))]
         [odd?
          (lambda (x)
            (and (not (= x 0))
                 (even? (- x 1))))])
  (list (even? 20) (odd? 20))) → (#t #f)

(letrec ([f (lambda () (+ x 2))]
         [x 1])
  (f)) → 3

(define list?
  (lambda (x)
    (letrec ([race
              (lambda (h t)
                (if (pair? h)
                    (let ([h (cdr h)])
                      (if (pair? h)
                          (and (not (eq? h t))
                               (race (cdr h) (cdr t)))
                          (null? h)))
                    (null? h)))])
      (race x x))))

#|+list2-def|#
(define list?
  (lambda (x)
    (let race ([h x] [t x])
      (if (pair? h)
          (let ([h (cdr h)])
            (if (pair? h)
                (and (not (eq? h t))
                     (race (cdr h) (cdr t)))
                (null? h)))
          (null? h)))))
#|-list2-def|#

#|+factorial|#
(define factorial
  (lambda (n)
    (let fact ([i n])
      (if (= i 0)
          1
          (* i (fact (- i 1)))))))

(factorial 0) → 1
(factorial 1) → 1
(factorial 2) → 2
(factorial 3) → 6
(factorial 10) → 3628800
#|-factorial|#

#|+factorial-iter|#
(define factorial
  (lambda (n)
    (let fact ([i n] [a 1])
      (if (= i 0)
          a
          (fact (- i 1) (* a i))))))
#|-factorial-iter|#

#|+fibonacci|#
(define fibonacci
  (lambda (n)
    (let fib ([i n])
      (cond
       [(= i 0) 0]
       [(= i 1) 1]
       [else (+ (fib (- i 1)) (fib (- i 2)))]))))

(fibonacci 0) → 0
(fibonacci 1) → 1
(fibonacci 2) → 1
(fibonacci 3) → 2
(fibonacci 4) → 3
(fibonacci 5) → 5
(fibonacci 6) → 8
(fibonacci 20) → 6765
(fibonacci 30) → 832040
#|-fibonacci|#

#|+fibonacci-acc|#
(define fibonacci
  (lambda (n)
    (if (= n 0)
        0
        (let fib ([i n] [a1 1] [a2 0])
          (if (= i 1)
              a1
              (fib (- i 1) (+ a1 a2) a1))))))
#|-fibonacci-acc|#

#|+factor|#
(define factor
  (lambda (n)
    (let f ([n n] [i 2])
      (cond
       [(>= i n) (list n)]
       [(integer? (/ n i))
        (cons i (f (/ n i) i))]
       [else (f n (+ i 1))]))))

(factor 0) → (0)
(factor 1) → (1)
(factor 12) → (2 2 3)
(factor 3628800) → (2 2 2 2 2 2 2 2 3 3 3 3 5 5 7)
(factor 9239) → (9239)
#|-factor|#

#|+call/cc-example-1|#
(call/cc
 (lambda (k)
   (* 5 4))) → 20

(call/cc
 (lambda (k)
   (* 5 (k 4)))) → 4

(+ 2
   (call/cc
    (lambda (k)
      (* 5 (k 4))))) → 6
#|-call/cc-example-1|#

#|+call/cc-product|#
(define product
  (lambda (ls)
    (call/cc
     (lambda (break)
       (let f ([ls ls])
         (cond
          [(null? ls) 1]
          [(= (car ls) 0) (break 0)]
          [else (* (car ls) (f (cdr ls)))]))))))

(product '(1 2 3 4 5)) → 120
(product '(7 3 8 0 1 9 5)) → 0
#|-call/cc-product|#

#|+call/cc-factorial|#
(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))
#|-call/cc-factorial|#

#|+call/cc-example-2|#
(factorial 4) → 24
(retry 1) → 24
(retry 2) → 48
#|-call/cc-example-2|#

#|+call/cc-example-3|#
(retry 2) → 48
(retry 5) → 120
#|-call/cc-example-3|#

#|+call/cc-example-4|#
(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk)))))

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))

(define pause
  (lambda ()
    (call/cc
     (lambda (k)
       (lwp (lambda () (k #f)))
       (start)))))
#|-call/cc-example-4|#

#|+call/cc-example-5|#
(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(start) → hey!
          hey!
          hey!
          hey!
          ...
#|-call/cc-example-5|#

#|+call/cc-style-1|#
(letrec ([f (lambda (x) (cons 'a x))]
         [g (lambda (x) (cons 'b (f x)))]
         [h (lambda (x) (g (cons 'c x)))])
  (cons 'd (h '()))) → (d b a c)
#|-call/cc-style-1|#

#|+call/cc-style-2|#
(letrec ([f (lambda (x k) (k (cons 'a x)))]
         [g (lambda (x k)
              (f x (lambda (v) (k (cons 'b v)))))]
         [h (lambda (x k) (g (cons 'c x) k))])
  (h '() (lambda (v) (cons 'd v))))
#|-call/cc-style-2|#

#|+call/cc-style-3|#
(define car&cdr
  (lambda (p k)
    (k (car p) (cdr p))))

(car&cdr '(a b c)
         (lambda (x y)
           (list y x))) → ((b c) a)
(car&cdr '(a b c) cons) → (a b c)
(car&cdr '(a b c a d) memv) → (a d)
#|-call/cc-style-3|#

#|+call/cc-style-4|#
(define integer-divide
  (lambda (x y success failure)
    (if (= y 0)
        (failure "divide by zero")
        (let ([q (quotient x y)])
          (success q (- x (* q y)))))))

(integer-divide 10 3 list (lambda (x) x)) → (3 1)
(integer-divide 10 0 list (lambda (x) x)) → "divide by zero"
#|-call/cc-style-4|#

#|+call/cc-style-5|#
(define product
  (lambda (ls k)
    (let ([break k])
      (let f ([ls ls] [k k])
        (cond
         [(null? ls) (k 1)]
         [(= (car ls) 0) (break 0)]
         [else (f (cdr ls)
                  (lambda (x)
                    (k (* (car ls) x))))])))))

(product '(1 2 3 4 5) (lambda (x) x)) → 120
(product '(7 3 8 0 1 9 5) (lambda (x) x)) → 0
#|-call/cc-style-5|#

#|+call/cc-style-6|#
(define reciprocals
  (lambda (ls)
    (call/cc
     (lambda (k)
       (map (lambda (x)
              (if (= x 0)
                  (k "zero found")
                  (/ 1 x)))
            ls)))))

(reciprocals '(2 1/3 5 1/4)) → (1/2 3 1/5 4)
(reciprocals '(2 1/3 0 5 1/4)) → "zero found"
#|-call/cc-style-6|#

#|+internal-definitions-1|#
(define f (lambda (x) (* x x)))
(let ([x 3])
  (define f (lambda (y) (+ y x)))
  (f 4)) → 7
(f 4) → 16
#|-internal-definitions-1|#

#|+internal-definitions-2|#
(let ()
  (define even?
    (lambda (x)
      (or (= x 0)
          (odd? (- x 1)))))
  (define odd?
    (lambda (x)
      (and (not (= x 0))
           (even? (- x 1)))))
  (even? 20)) → #t
#|-internal-definitions-2|#

#|+internal-definitions-3|#
(define list?
  (lambda (x)
    (define race
      (lambda (h t)
        (if (pair? h)
            (let ([h (cdr h)])
              (if (pair? h)
                  (and (not (eq? h t))
                       (race (cdr h) (cdr t)))
                  (null? h)))
            (null? h))))
    (race x x)))
#|-internal-definitions-3|#

#|+internal-definitions-4|#
(let ([x 3])
  (define-syntax set-x!
    (syntax-rules ()
      [(_ e) (set! x e)]))
  (set-x! (+ x x))
  x) → 6
#|-internal-definitions-4|#

#|+internal-definitions-5|#
(define calc #f)
(let ()
  (define do-calc
    (lambda (ek expr)
      (cond
       [(number? expr) expr]
       [(and (list? expr) (= (length expr) 3))
        (let ([op (car expr)] [args (cdr expr)])
          (case op
            [(add) (apply-op ek + args)]
            [(sub) (apply-op ek - args)]
            [(mul) (apply-op ek * args)]
            [(div) (apply-op ek / args)]
            [else (complain ek "invalid operator" op)]))]
       [else (complain ek "invalid expression" expr)])))
  (define apply-op
    (lambda (ek op args)
      (op (do-calc ek (car args)) (do-calc ek (cadr args)))))
  (define complain
    (lambda (ek msg expr)
      (ek (list msg expr))))
  (set! calc
        (lambda (expr)
          ; grab an error continuation ek
          (call/cc
           (lambda (ek)
             (do-calc ek expr))))))

(calc '(add (mul 3 2) -4)) → 2
(calc '(div 1/2 1/6)) → 3
(calc '(add (mul 3 2) (div 4))) → ("invalid expression" (div 4))
(calc '(mul (add 1 -2) (pow 2 7))) → ("invalid operator" pow)
#|-internal-definitions-5|#

#|+internal-definitions-6|#
(let ([temp op])
  (cond
   [(memv temp '(add)) (apply-op ek + args)]
   [(memv temp '(sub)) (apply-op ek - args)]
   [(memv temp '(mul)) (apply-op ek * args)]
   [(memv temp '(div)) (apply-op ek / args)]
   [else (complain ek "invalid operator" op)]))
#|-internal-definitions-6|#

#|+libraries-1|#
(library (grades)
  (export gpa->grade gpa)
  (import (rnrs))

  (define in-range?
    (lambda (x n y)
      (and (>= n x) (< n y))))

  (define-syntax range-case
    (syntax-rules (- else)
      [(_ expr ((x - y) e1 e2 ...) ... [else ee1 ee2 ...])
       (let ([tmp expr])
         (cond
          [(in-range? x tmp y) e1 e2 ...]
          ...
          [else ee1 ee2 ...]))]
      [(_ expr ((x - y) e1 e2 ...) ...)
       (let ([tmp expr])
         (cond
          [(in-range? x tmp y) e1 e2 ...]
          ...))]))

  (define letter->number
    (lambda (x)
      (case x
        [(a)  4.0]
        [(b)  3.0]
        [(c)  2.0]
        [(d)  1.0]
        [(f)  0.0]
        [else (assertion-violation 'grade "invalid letter grade" x)])))

  (define gpa->grade
    (lambda (x)
      (range-case x
                  [(0.0 - 0.5) 'f]
                  [(0.5 - 1.5) 'd]
                  [(1.5 - 2.5) 'c]
                  [(2.5 - 3.5) 'b]
                  [else 'a])))

  (define-syntax gpa
    (syntax-rules ()
      [(_ g1 g2 ...)
       (let ([ls (map letter->number '(g1 g2 ...))])
         (/ (apply + ls) (length ls)))])))
#|-libraries-1|#

#|+libraries-2|#
(import (grades))
(gpa c a c b b) → 2.8
(gpa->grade 2.8) → b
#|-libraries-2|#

#|+libraries-3|#
(import (grades))
(gpa a x b c) → 3.0
#|-libraries-3|#
