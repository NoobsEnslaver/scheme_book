;; -*- coding: utf-8; -*-

#|+variables-bindings-2|#
(define f
  (lambda (x)
    (g x)))
(define g
  (lambda (x)
    (+ x x)))
#|-variables-bindings-2|#

#|+variables-bindings-3|#
(define q (g 3))
(define g
  (lambda (x)
    (+ x x)))
#|-variables-bindings-3|#

(define make-list
  (case-lambda
    [(n) (make-list n #f)]
    [(n x)
     (do ([n n (- n 1)] [ls '() (cons x ls)])
         ((zero? n) ls))]))

(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))

(define substring2
  (case-lambda
    [(s) (substring2 s 0 (string-length s))]
    [(s end) (substring2 s 0 end)]
    [(s start end) (substring s start end)]))

(define substring3
  (case-lambda
    [(s) (substring3 s 0 (string-length s))]
    [(s start end) (substring s start end)]))

#|+local-bindings-1|#
(let ([x (* 3.0 3.0)] [y (* 4.0 4.0)])
  (sqrt (+ x y))) → 5.0

(let ([x 'a] [y '(b c)])
  (cons x y)) → (a b c)

(let ([x 0] [y 1])
  (let ([x y] [y x])
    (list x y))) → (1 0)
#|-local-bindings-1|#

(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

#|+local-bindings-2|#
(let* ([x (* 5.0 5.0)]
       [y (- x (* 4.0 4.0))])
  (sqrt y)) → 3.0

(let ([x 0] [y 1])
  (let* ([x y] [y x])
    (list x y))) → (1 1)
#|-local-bindings-2|#

(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...)
     (let () e1 e2 ...)]
    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
     (let ((x1 v1))
       (let* ((x2 v2) ...) e1 e2 ...))]))

#|+local-bindings-3|#
(letrec ([sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))])
  (sum 5)) → 15
#|-local-bindings-3|#

#|+local-bindings-4|#
(letrec* ([sum (lambda (x)
                 (if (zero? x)
                     0
                     (+ x (sum (- x 1)))))]
          [f (lambda () (cons n n-sum))]
          [n 15]
          [n-sum (sum n)])
  (f)) → (15 . 120)

(letrec* ([f (lambda () (lambda () g))]
          [g (f)])
  (eq? (g) g)) → #t

(letrec* ([g (f)]
          [f (lambda () (lambda () g))])
  (eq? (g) g)) → exception: attempt to reference undefined variable f
#|-local-bindings-4|#

#|+multiple-values-1|#
(let-values ([(a b) (values 1 2)] [c (values 1 2 3)])
  (list a b c)) → (1 2 (1 2 3))

(let*-values ([(a b) (values 1 2)] [(a b) (values b a)])
  (list a b)) → (2 1)
#|-multiple-values-1|#

#|+variable-definitions-1|#
(define x 3)
x → 3

(define f
  (lambda (x y)
    (* (+ x y) 2)))
(f 5 4) → 18

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))
(sum-of-squares 3 4) → 25

(define f
  (lambda (x)
    (+ x 1)))
(let ([x 2])
  (define f
    (lambda (y)
      (+ y x)))
  (f 3)) → 5
(f 3) → 4
#|-variable-definitions-1|#

#|+variable-definitions-2|#
(define-syntax multi-define-syntax
  (syntax-rules ()
    [(_ (var expr) ...)
     (begin
       (define-syntax var expr)
       ...)]))
(let ()
  (define plus
    (lambda (x y)
        (if (zero? x)
            y
            (plus (sub1 x) (add1 y)))))
  (multi-define-syntax
    (add1 (syntax-rules () [(_ e) (+ e 1)]))
    (sub1 (syntax-rules () [(_ e) (- e 1)])))
  (plus 7 8)) → 15
#|-variable-definitions-2|#

(define f
  (lambda (x)
    (g x)))

#|+variable-definitions-3|#
(define g
  (lambda (x)
    (+ x x)))
(f 3) → 6
#|-variable-definitions-3|#

#|+assignment-1|#
(define flip-flop
  (let ([state #f])
    (lambda ()
      (set! state (not state))
      state)))

(flip-flop) → #t
(flip-flop) → #f
(flip-flop) → #t
#|-assignment-1|#

#|+assignment-2|#
(define memoize
  (lambda (proc)
    (let ([cache '()])
      (lambda (x)
        (cond
         [(assq x cache) => cdr]
         [else
          (let ([ans (proc x)])
            (set! cache (cons (cons x ans) cache))
            ans)])))))

(define fibonacci
  (memoize
   (lambda (n)
     (if (< n 2)
         1
         (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

(fibonacci 100) → 573147844013817084101
#|-assignment-2|#
