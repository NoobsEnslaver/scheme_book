;; -*- coding: utf-8; -*-
"Hi Mom!" → "Hi Mom!"

"hello" → "hello"
42 → 42
22/7 → 22/7
3.141592653 → 3.141592653
+ → #<procedure>
(+ 76 31) → 107
(* -12 10) → -120
'(a b c d) → (a b c d)

(car '(a b c)) → a
(cdr '(a b c)) → (b c)
(cons 'a '(b c)) → (a b c)
(cons (car '(a b c))
      (cdr '(d e f))) → (a e f)

(define square
  (lambda (n)
    (* n n)))

(square 5) → 25
(square -200) → 40000
(square 0.5) → 0.25
(square -1/2) → 1/4

(define reciprocal
  (lambda (n)
    (if (= n 0)
        "oops!"
        (/ 1 n))))

(reciprocal 10) → 1/10
(reciprocal 1/10) → 10
(reciprocal 0) → "oops!"
(reciprocal (reciprocal 1/10)) → 1/10

123456789987654321 → 123456789987654321
3/4 → 3/4
2.718281828 → 2.718281828
2.2+1.1i → 2.2+1.1i

(+ 1/2 1/2) → 1
(- 1.5 1/2) → 1.0

(* 3 1/2) → 3/2
(/ 1.5 3/4) → 2.0

(+ (+ 2 2) (+ 2 2)) → 8
(- 2 (* 4 1/3)) → 2/3
(* 2 (* 2 (* 2 (* 2 2)))) → 32
(/ (* 6/7 7/2) (- 4.5 1.5)) → 1.0

(quote (1 2 3 4 5)) → (1 2 3 4 5)
(quote ("это" "всё" "ещё" "список")) → ("это" "всё" "ещё" "список")
(quote (+ 3 4)) → (+ 3 4)

'(1 2 3 4) → (1 2 3 4)
'((1 2) (3 4)) → ((1 2) (3 4))
'(/ (* 2 -1) 3) → (/ (* 2 -1) 3)

(quote hello) → hello

'2 → 2
'2/3 → 2/3
(quote "Hi Mom!") → "Hi Mom!"

(car '(a b c)) → a
(cdr '(a b c)) → (b c)
(cdr '(a)) → ()

(car (cdr '(a b c))) → b
(cdr (cdr '(a b c))) → (c)

(car '((a b) (c d))) → (a b)
(cdr '((a b) (c d))) → ((c d))

(cons 'a '()) → (a)
(cons 'a '(b c)) → (a b c)
(cons 'a (cons 'b (cons 'c '()))) → (a b c)
(cons '(a b) '(c d)) → ((a b) c d)

(car (cons 'a '(b c))) → a
(cdr (cons 'a '(b c))) → (b c)
(cons (car '(a b c))
      (cdr '(d e f))) → (a e f)
(cons (car '(a b c))
      (cdr '(a b c))) → (a b c)

(cons 'a 'b) → (a . b)
(cdr '(a . b)) → b
(cons 'a '(b . c)) → (a b . c)

'(a . (b . (c . ()))) → (a b c)

(list 'a 'b 'c) → (a b c)
(list 'a) → (a)
(list) → ()

(let ((x 2))
  (+ x 3)) → 5

(let ((y 3))
  (+ 2 y)) → 5

(let ((x 2) (y 3))
  (+ x y)) → 5

(+ (* 4 4) (* 4 4)) → 32

(let ((a (* 4 4))) (+ a a)) → 32

(let ([list1 '(a b c)] [list2 '(d e f)])
  (cons (cons (car list1)
              (car list2))
        (cons (car (cdr list1))
              (car (cdr list2))))) → ((a . d) b . e)

(let ([f +])
  (f 2 3)) → 5

(let ([f +] [x 2])
  (f x 3)) → 5

(let ([f +] [x 2] [y 3])
  (f x y)) → 5

(let ([+ *])
  (+ 2 3)) → 6

(+ 2 3) → 5

(let ([a 4] [b -3])
      (let ([a-squared (* a a)]
            [b-squared (* b b)])
        (+ a-squared b-squared))) → 25

(let ([x 1])
  (let ([x (+ x 1)])
    (+ x x))) → 4

(let ([x 1])
  (let ([new-x (+ x 1)])
    (+ new-x new-x))) → 4

((lambda (x) (+ x x)) (* 3 4)) → 24

(let ([double (lambda (x) (+ x x))])
  (list (double (* 3 4))
        (double (/ 99 11))
        (double (- 2 7)))) → (24 18 -10)

(let ([double-cons (lambda (x) (cons x x))])
  (double-cons 'a)) → (a . a)

(let ([double-any (lambda (f x) (f x x))])
  (list (double-any + 13)
        (double-any cons 'a))) → (26 (a . a))

(let ([x 'a])
  (let ([f (lambda (y) (list x y))])
    (f 'b))) → (a b)

(let ([f (let ([x 'sam])
           (lambda (y z) (list x y z)))])
  (f 'i 'am)) → (sam i am)

(let ([f (let ([x 'sam])
           (lambda (y z) (list x y z)))])
  (let ([x 'not-sam])
    (f 'i 'am))) → (sam i am)

(let ([f (lambda x x)])
  (f 1 2 3 4)) → (1 2 3 4)

(let ([f (lambda x x)])
  (f)) → ()

(let ([g (lambda (x . y) (list x y))])
  (g 1 2 3 4)) → (1 (2 3 4))

(let ([h (lambda (x y . z) (list x y z))])
  (h 'a 'b 'c 'd)) → (a b (c d))

(define double-any
  (lambda (f x)
    (f x x)))

(double-any + 10) → 20
(double-any cons 'a) → (a . a)

(define sandwich "peanut-butter-and-jelly")

sandwich → "peanut-butter-and-jelly"

(define xyz '(x y z))
(let ([xyz '(z y x)])
  xyz) → (z y x)

(define list (lambda x x))

(define cadr
  (lambda (x)
    (car (cdr x))))

(define cddr
  (lambda (x)
    (cdr (cdr x))))

(cadr '(a b c)) → b
(cddr '(a b c)) → (c)

(define (cadr x)
   (car (cdr x)))

(define (list . x) x)

(define doubler
  (lambda (f)
    (lambda (x) (f x x))))

(define double (doubler +))
(double 13/2) → 13

(define double-cons (doubler cons))
(double-cons 'a) → (a . a)

(define double-any
  (lambda (f x)
    ((doubler f) x)))

(define proc1
  (lambda (x y)
    (proc2 y x)))

(define proc2 cons)
(proc1 'a 'b) → (b . a)

(define abs
  (lambda (n)
    (if (< n 0)
        (- 0 n)
        n)))

(abs 77) → 77
(abs -77) → 77

(define abs
  (lambda (n)
    (if (>= n 0)
        n
        (- 0 n))))

(define abs
  (lambda (n)
    (if (not (< n 0))
        n
        (- 0 n))))

(define abs
  (lambda (n)
    (if (or (> n 0) (= n 0))
        n
        (- 0 n))))

(define abs
  (lambda (n)
    (if (= n 0)
        0
        (if (< n 0)
            (- 0 n)
            n))))

(define abs
  (lambda (n)
    ((if (>= n 0) + -)
     0
     n)))

(define reciprocal
  (lambda (n)
    (if (= n 0)
        "oops!"
        (/ 1 n))))

(< -1 0) → #t
(> -1 0) → #f

(if #t 'true 'false) → true
(if #f 'true 'false) → false
(if '() 'true 'false) → true
(if 1 'true 'false) → true
(if '(a b c) 'true 'false) → true

(not #t) → #f
(not "false") → #f
(not #f) → #t

(or) → #f
(or #f) → #f
(or #f #t) → #t
(or #f 'a #f) → a

(define reciprocal
  (lambda (n)
    (and (not (= n 0))
         (/ 1 n))))

(reciprocal 3) → 1/3
(reciprocal 0.5) → 2.0
(reciprocal 0) → #f

(null? '()) → #t
(null? 'abc) → #f
(null? '(x y z)) → #f
(null? (cdddr '(x y z))) → #t

(define lisp-cdr
  (lambda (x)
    (if (null? x)
        '()
        (cdr x))))

(lisp-cdr '(a b c)) → (b c)
(lisp-cdr '(c)) → ()
(lisp-cdr '()) → ()

(eqv? 'a 'a) → #t
(eqv? 'a 'b) → #f
(eqv? #f #f) → #t
(eqv? #t #t) → #t
(eqv? #f #t) → #f
(eqv? 3 3) → #t
(eqv? 3 2) → #f
(let ([x "Hi Mom!"])
  (eqv? x x)) → #t
(let ([x (cons 'a 'b)])
  (eqv? x x)) → #t
(eqv? (cons 'a 'b) (cons 'a 'b)) → #f

(pair? '(a . c)) → #t
(pair? '(a b c)) → #t
(pair? '()) → #f
(pair? 'abc) → #f
(pair? "Hi Mom!") → #f
(pair? 1234567890) → #f

(define reciprocal
  (lambda (n)
    (if (and (number? n) (not (= n 0)))
        (/ 1 n)
        "oops!")))

(reciprocal 2/3) → 3/2
(reciprocal 'a) → "oops!"

(define reciprocal
  (lambda (n)
    (if (and (number? n) (not (= n 0)))
        (/ 1 n)
        (assertion-violation 'reciprocal
                             "improper argument"
                             n))))

(reciprocal .25) → 4.0
(reciprocal 0) → exception in reciprocal: improper argument 0
(reciprocal 'a) → exception in reciprocal: improper argument a

(define sign
  (lambda (n)
    (if (< n 0)
        -1
        (if (> n 0)
            +1
            0))))

(sign -88.3) → -1
(sign 0) → 0
(sign 333333333333) → 1
(* (sign -88.3) (abs -88.3)) → -88.3

(define sign
  (lambda (n)
    (cond
     [(< n 0) -1]
     [(> n 0) +1]
     [else 0])))

(define sign
  (lambda (n)
    (cond
     [(< n 0) -1]
     [(> n 0) +1]
     [(= n 0) 0])))

(define income-tax
  (lambda (income)
    (cond
     [(<= income 10000) (* income .05)]
     [(<= income 20000) (+ (* (- income 10000) .08) 500.00)]
     [(<= income 30000) (+ (* (- income 20000) .13) 1300.00)]
     [else (+ (* (- income 30000) .21) 2600.00)])))

(income-tax 5000) → 250.0
(income-tax 15000) → 900.0
(income-tax 25000) → 1950.0
(income-tax 50000) → 6800.0

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))

(length '()) → 0
(length '(a)) → 1
(length '(a b)) → 2

(list-copy '()) → ()
(list-copy '(a b c)) → (a b c)

(define list-copy
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls)
              (list-copy (cdr ls))))))

(define memv
  (lambda (x ls)
    (cond
     [(null? ls) #f]
     [(eqv? (car ls) x) ls]
     [else (memv x (cdr ls))])))

(memv 'a '(a b b d)) → (a b b d)
(memv 'b '(a b b d)) → (b b d)
(memv 'c '(a b b d)) → #f
(memv 'd '(a b b d)) → (d)
(if (memv 'b '(a b b d))
    "yes"
    "no") → "yes"

(define remv
  (lambda (x ls)
    (cond
     [(null? ls) '()]
     [(eqv? (car ls) x) (remv x (cdr ls))]
     [else (cons (car ls) (remv x (cdr ls)))])))

(remv 'a '(a b b d)) → (b b d)
(remv 'b '(a b b d)) → (a d)
(remv 'c '(a b b d)) → (a b b d)
(remv 'd '(a b b d)) → (a b b)

(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))

(tree-copy '((a . b) . c)) → ((a . b) . c)

(define abs-all
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (abs (car ls))
              (abs-all (cdr ls))))))

(abs-all '(1 -2 3 -4 5 -6)) → (1 2 3 4 5 6)

(define abs-all
  (lambda (ls)
    (map abs ls)))

(map abs '(1 -2 3 -4 5 -6)) → (1 2 3 4 5 6)

(map (lambda (x) (* x x))
     '(1 -3 -5 7)) → (1 9 25 49)

(map cons '(a b c) '(1 2 3)) → ((a . 1) (b . 2) (c . 3))

(define map1
  (lambda (p ls)
    (if (null? ls)
        '()
        (cons (p (car ls))
              (map1 p (cdr ls))))))

(map1 abs '(1 -2 3 -4 5 -6)) → (1 2 3 4 5 6)

(define abcde '(a b c d e))
abcde → (a b c d e)
(set! abcde (cdr abcde))
abcde → (b c d e)
(let ([abcde '(a b c d e)])
  (set! abcde (reverse abcde))
  abcde) → (e d c b a)

(define quadratic-formula
  (lambda (a b c)
    (let ([root1 0] [root2 0] [minusb 0] [radical 0] [divisor 0])
      (set! minusb (- 0 b))
      (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
      (set! divisor (* 2 a))
      (set! root1 (/ (+ minusb radical) divisor))
      (set! root2 (/ (- minusb radical) divisor))
      (cons root1 root2))))

(quadratic-formula 2 -4 -6) → (3 . -1)

(define quadratic-formula
  (lambda (a b c)
    (let ([minusb (- 0 b)]
          [radical (sqrt (- (* b b) (* 4 (* a c))))]
          [divisor (* 2 a)])
      (let ([root1 (/ (+ minusb radical) divisor)]
            [root2 (/ (- minusb radical) divisor)])
        (cons root1 root2)))))

(define kons-count 0)
(define kons
  (lambda (x y)
    (set! kons-count (+ kons-count 1))
    (cons x y)))

(kons 'a '(b c)) → (a b c)
kons-count → 1
(kons 'a (kons 'b (kons 'c '()))) → (a b c)
kons-count → 4

(define next 0)
(define count
  (lambda ()
    (let ([v next])
      (set! next (+ next 1))
      v)))

(count) → 0
(count) → 1

(define count
  (let ([next 0])
    (lambda ()
      (let ([v next])
        (set! next (+ next 1))
        v))))

(define make-counter
  (lambda ()
    (let ([next 0])
      (lambda ()
        (let ([v next])
          (set! next (+ next 1))
          v)))))

(define count1 (make-counter))
(define count2 (make-counter))

(count1) → 0
(count2) → 0
(count1) → 1
(count1) → 2
(count2) → 1

(define shhh #f)
(define tell #f)
(let ([secret 0])
  (set! shhh
        (lambda (message)
          (set! secret message)))
  (set! tell
        (lambda ()
          secret)))

(shhh "sally likes harry")
(tell) → "sally likes harry"
secret → exception: variable secret is not bound

(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      (lambda ()
        (if (not flag)
            (begin (set! val (t))
                   (set! flag #t)))
        val))))

(define p
  (lazy (lambda ()
          (display "Ouch!")
          (newline)
          "got me")))

(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
         [(eqv? msg 'empty?) (null? ls)]
         [(eqv? msg 'push!) (set! ls (cons (car args) ls))]
         [(eqv? msg 'top) (car ls)]
         [(eqv? msg 'pop!) (set! ls (cdr ls))]
         [else "oops"])))))

(define stack1 (make-stack))
(define stack2 (make-stack))
(list (stack1 'empty?) (stack2 'empty?)) → (#t #t)

(stack1 'push! 'a)
(list (stack1 'empty?) (stack2 'empty?)) → (#f #t)

(stack1 'push! 'b)
(stack2 'push! 'c)
(stack1 'top) → b
(stack2 'top) → c

(stack1 'pop!)
(stack1 'top) → a
(list (stack1 'empty?) (stack2 'empty?)) → (#f #f)

(stack1 'pop!)
(list (stack1 'empty?) (stack2 'empty?)) → (#t #f)

(define p (list 1 2 3))
(set-car! (cdr p) 'two)
p → (1 two 3)
(set-cdr! p '())
p → (1)

(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

(define getq
  (lambda (q)
    (car (car q))))

(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))

(define myq (make-queue))

(putq! myq 'a)
(putq! myq 'b)
(getq myq) → a
(delq! myq)
(getq myq) → b
(delq! myq)
(putq! myq 'c)
(putq! myq 'd)
(getq myq) → c
(delq! myq)
(getq myq) → d
