#|+procedure-application-1|#
(+ 3 4) → 7

((if (odd? 3) + -) 6 2) → 8

((lambda (x) x) 5) → 5

(let ([f (lambda (x) (+ x x))])
  (f 8)) → 16
#|-procedure-application-1|#

#|+procedure-application-2|#
(apply + '(4 5)) → 9

(apply min '(6 8 3 2 5)) → 2

(apply min  5 1 3 '(6 8 3 2 5)) → 1

(apply vector 'a 'b '(c d e)) → #(a b c d e)

(define first
  (lambda (ls)
    (apply (lambda (x . y) x) ls)))
(define rest
  (lambda (ls)
    (apply (lambda (x . y) y) ls)))
(first '(a b c d)) → a
(rest '(a b c d)) → (b c d)

(apply append
  '(1 2 3)
  '((a b) (c d e) (f))) → (1 2 3 a b c d e f)
#|-procedure-application-2|#

#|+sequencing-1|#
(define x 3)
(begin
  (set! x (+ x 1))
  (+ x x)) → 8
#|-sequencing-1|#

#|+sequencing-2|#
(let ()
  (begin (define x 3) (define y 4))
  (+ x y)) → 7
#|-sequencing-2|#

#|+sequencing-3|#
(define swap-pair!
  (lambda (x)
    (let ([temp (car x)])
      (set-car! x (cdr x))
      (set-cdr! x temp)
      x)))
(swap-pair! (cons 'a 'b)) → (b . a)
#|-sequencing-3|#

#|+conditionals-1|#
(let ([ls '(a b c)])
  (if (null? ls)
      '()
      (cdr ls))) → (b c)

(let ([ls '()])
  (if (null? ls)
      '()
      (cdr ls))) → ()

(let ([abs
       (lambda (x)
         (if (< x 0)
             (- 0 x)
             x))])
  (abs -4)) → 4

(let ([x -4])
  (if (< x 0)
      (list 'minus (- 0 x))
      (list 'plus 4))) → (minus 4)
#|-conditionals-1|#

#|+conditionals-2|#
(not #f) → #t
(not #t) → #f
(not '()) → #f
(not (< 4 5)) → #f
#|-conditionals-2|#

#|+conditionals-3|#
(let ([x 3])
  (and (> x 2) (< x 4))) → #t

(let ([x 5])
  (and (> x 2) (< x 4))) → #f

(and #f '(a b) '(c d)) → #f
(and '(a b) '(c d) '(e f)) → (e f)
#|-conditionals-3|#

#|+conditionals-4|#
(let ([x 3])
  (or (< x 2) (> x 4))) → #f

(let ([x 5])
  (or (< x 2) (> x 4))) → #t

(or #f '(a b) '(c d)) → (a b)
#|-conditionals-4|#

#|+conditionals-5|#
(let ([x 0])
  (cond
    [(< x 0) (list 'minus (abs x))]
    [(> x 0) (list 'plus x)]
    [else (list 'zero x)])) → (zero 0)

(define select
  (lambda (x)
    (cond
      [(not (symbol? x))]
      [(assq x '((a . 1) (b . 2) (c . 3))) => cdr]
      [else 0])))

(select 3) → #t
(select 'b) → 2
(select 'e) → 0
#|-conditionals-5|#

#|+conditionals-6|#
(let ([x -4] [sign 'plus])
  (when (< x 0)
    (set! x (- 0 x))
    (set! sign 'minus))
  (list sign x)) → (minus 4)

(define check-pair
  (lambda (x)
    (unless (pair? x)
      (syntax-violation 'check-pair "invalid argument" x))
    x))

(check-pair '(a b c)) → (a b c)
#|-conditionals-6|#

(define-syntax when
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (if e0 (begin e1 e2 ...))]))

(define-syntax unless
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (if (not e0) (begin e1 e2 ...))]))

#|+conditionals-7|#
(define-syntax unless
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (when (not e0) e1 e2 ...)]))
#|-conditionals-7|#

#|+conditionals-8|#
(let ([x 4] [y 5])
  (case (+ x y)
    [(1 3 5 7 9) 'odd]
    [(0 2 4 6 8) 'even]
    [else 'out-of-range])) → odd
#|-conditionals-8|#

#|+iterations-1|#
(define divisors
  (lambda (n)
    (let f ([i 2])
      (cond
        [(>= i n) '()]
        [(integer? (/ n i)) (cons i (f (+ i 1)))]
        [else (f (+ i 1))]))))

(divisors 5) → ()
(divisors 32) → (2 4 8 16)
#|-iterations-1|#

(define divisors
  (lambda (n)
    (let f ([i 2] [ls '()])
      (cond
        [(>= i n) ls]
        [(integer? (/ n i)) (f (+ i 1) (cons i ls))]
        [else (f (+ i 1) ls)]))))

#|+iterations-2|#
(define factorial
  (lambda (n)
    (do ([i n (- i 1)] [a 1 (* a i)])
        ((zero? i) a))))

(factorial 10) → 3628800

(define fibonacci
  (lambda (n)
    (if (= n 0)
        0
        (do ([i n (- i 1)] [a1 1 (+ a1 a2)] [a2 0 a1])
            ((= i 1) a1)))))

(fibonacci 6) → 8
#|-iterations-2|#

#|+iterations-3|#
(define divisors
  (lambda (n)
    (do ([i 2 (+ i 1)]
         [ls '()
             (if (integer? (/ n i))
                 (cons i ls)
                 ls)])
        ((>= i n) ls))))
#|-iterations-3|#

#|+iterations-4|#
(define scale-vector!
  (lambda (v k)
    (let ([n (vector-length v)])
      (do ([i 0 (+ i 1)])
          ((= i n))
        (vector-set! v i (* (vector-ref v i) k))))))

(define vec (vector 1 2 3 4 5))
(scale-vector! vec 2)
vec → #(2 4 6 8 10)
#|-iterations-4|#

#|+mapping-1|#
(map abs '(1 -2 3 -4 5 -6)) → (1 2 3 4 5 6)

(map (lambda (x y) (* x y))
     '(1 2 3 4)
     '(8 7 6 5)) → (8 14 18 20)
#|-mapping-1|#

(define map
  (lambda (f ls . more)
    (if (null? more)
        (let map1 ([ls ls])
          (if (null? ls)
              '()
              (cons (f (car ls))
                    (map1 (cdr ls)))))
        (let map-more ([ls ls] [more more])
          (if (null? ls)
              '()
              (cons
               (apply f (car ls) (map car more))
               (map-more (cdr ls) (map cdr more))))))))

#|+mapping-2|#
(define for-each
  (lambda (f ls . more)
    (do ([ls ls (cdr ls)] [more more (map cdr more)])
        ((null? ls))
      (apply f (car ls) (map car more)))))

(let ([same-count 0])
  (for-each
    (lambda (x y)
      (when (= x y)
        (set! same-count (+ same-count 1))))
    '(1 2 3 4 5 6)
    '(2 3 3 4 7 6))
  same-count) → 3
#|-mapping-2|#

#|+mapping-3|#
(exists symbol? '(1.0 #\a "hi" '())) → #f

(exists member
        '(a b c)
        '((c b) (b a) (a c))) → (b a)

(exists (lambda (x y z) (= (+ x y) z))
        '(1 2 3 4)
        '(1.2 2.3 3.4 4.5)
        '(2.3 4.4 6.4 8.6)) → #t
#|-mapping-3|#

(define exists
  (lambda (f ls . more)
    (and (not (null? ls))
      (let exists ([x (car ls)] [ls (cdr ls)] [more more])
        (if (null? ls)
            (apply f x (map car more))
            (or (apply f x (map car more))
                (exists (car ls) (cdr ls) (map cdr more))))))))

#|+mapping-4|#
(for-all symbol? '(a b c d)) → #t

(for-all =
         '(1 2 3 4)
         '(1.0 2.0 3.0 4.0)) → #t

(for-all (lambda (x y z) (= (+ x y) z))
         '(1 2 3 4)
         '(1.2 2.3 3.4 4.5)
         '(2.2 4.3 6.5 8.5)) → #f
#|-mapping-4|#

(define for-all
  (lambda (f ls . more)
    (or (null? ls)
        (let for-all ([x (car ls)] [ls (cdr ls)] [more more])
          (if (null? ls)
              (apply f x (map car more))
              (and (apply f x (map car more))
                   (for-all (car ls) (cdr ls) (map cdr more))))))))

#|+mapping-5|#
(fold-left cons '() '(1 2 3 4)) → ((((() . 1) . 2) . 3) . 4)

(fold-left
  (lambda (a x) (+ a (* x x)))
  0 '(1 2 3 4 5)) → 55

(fold-left
  (lambda (a . args) (append args a))
  '(question)
  '(that not to)
  '(is to be)
  '(the be: or)) → (to be or not to be: that is the question)
#|-mapping-5|#

#|+mapping-6|#
(fold-right cons '() '(1 2 3 4)) → (1 2 3 4)

(fold-right
  (lambda (x a) (+ a (* x x)))
  0 '(1 2 3 4 5)) → 55

(fold-right
  (lambda (x y a) (cons* x y a))   → (parting is such sweet sorrow
  '((with apologies))                gotta go see ya tomorrow
  '(parting such sorrow go ya)       (with apologies))
  '(is sweet gotta see tomorrow))
#|-mapping-6|#

#|+mapping-7|#
(vector-map abs '#(1 -2 3 -4 5 -6)) → #(1 2 3 4 5 6)
(vector-map (lambda (x y) (* x y))
  '#(1 2 3 4)
  '#(8 7 6 5)) → #(8 14 18 20)
#|-mapping-7|#

#|+mapping-8|#
(let ([same-count 0])
  (vector-for-each
    (lambda (x y)
      (when (= x y)
        (set! same-count (+ same-count 1))))
    '#(1 2 3 4 5 6)
    '#(2 3 3 4 7 6))
  same-count) → 3
#|-mapping-8|#

#|+mapping-9|#
(let ([ls '()])
  (string-for-each
    (lambda r (set! ls (cons r ls)))
    "abcd"
    "===="
    "1234")
  (map list->string (reverse ls))) → ("a=1" "b=2" "c=3" "d=4")
#|-mapping-9|#

#|+continuations-1|#
(define member
  (lambda (x ls)
    (call/cc
      (lambda (break)
        (do ([ls ls (cdr ls)])
            ((null? ls) #f)
          (when (equal? x (car ls))
            (break ls)))))))

(member 'd '(a b c)) → #f
(member 'b '(a b c)) → (b c)
#|-continuations-1|#

#|+continuations-2|#
(let ([p (open-input-file "input-file")])
  (dynamic-wind
    (lambda () #f)
    (lambda () (process p))
    (lambda () (close-port p))))
#|-continuations-2|#

#|+continuations-3|#
(define-syntax unwind-protect
  (syntax-rules ()
    [(_ body cleanup ...)
     (dynamic-wind
       (lambda () #f)
       (lambda () body)
       (lambda () cleanup ...))]))

((call/cc
   (let ([x 'a])
     (lambda (k)
       (unwind-protect
         (k (lambda () x))
         (set! x 'b)))))) → b
#|-continuations-3|#

(define-syntax fluid-let
  (syntax-rules ()
    [(_ ((x e)) b1 b2 ...)
     (let ([y e])
       (let ([swap (lambda () (let ([t x]) (set! x y) (set! y t)))])
         (dynamic-wind swap (lambda () b1 b2 ...) swap)))]))

#|+continuations-4|#
(let ([x 3])
  (+ (fluid-let ([x 5])
       x)
     x)) → 8
#|-continuations-4|#

#|+continuations-5|#
(let ([x 'a])
  (let ([f (lambda () x)])
    (cons (call/cc
            (lambda (k)
              (fluid-let ([x 'b])
                (k (f)))))
          (f)))) → (b . a)
#|-continuations-5|#

#|+continuations-6|#
(define reenter #f)
(define x 0)
(fluid-let ([x 1])
  (call/cc (lambda (k) (set! reenter k)))
  (set! x (+ x 1))
  x) → 2
x → 0
(reenter '*) → 3
(reenter '*) → 4
x → 0
#|-continuations-6|#

#|+continuations-7|#
(library (dynamic-wind)
  (export dynamic-wind call/cc
    (rename (call/cc call-with-current-continuation)))
  (import (rename (except (rnrs) dynamic-wind) (call/cc rnrs:call/cc)))

  (define winders '())

  (define common-tail
    (lambda (x y)
      (let ([lx (length x)] [ly (length y)])
        (do ([x (if (> lx ly) (list-tail x (- lx ly)) x) (cdr x)]
             [y (if (> ly lx) (list-tail y (- ly lx)) y) (cdr y)])
            ((eq? x y) x)))))

  (define do-wind
    (lambda (new)
      (let ([tail (common-tail new winders)])
        (let f ([ls winders])
          (if (not (eq? ls tail))
              (begin
                (set! winders (cdr ls))
                ((cdar ls))
                (f (cdr ls)))))
        (let f ([ls new])
          (if (not (eq? ls tail))
              (begin
                (f (cdr ls))
                ((caar ls))
                (set! winders ls)))))))

  (define call/cc
    (lambda (f)
      (rnrs:call/cc
        (lambda (k)
          (f (let ([save winders])
               (lambda (x)
                 (unless (eq? save winders) (do-wind save))
                 (k x))))))))

  (define dynamic-wind
    (lambda (in body out)
      (in)
      (set! winders (cons (cons in out) winders))
      (let-values ([ans* (body)])
        (set! winders (cdr winders))
        (out)
        (apply values ans*)))))
#|-continuations-7|#

#|+delayed-evaluation-1|#
(define stream-car
  (lambda (s)
    (car (force s))))

(define stream-cdr
  (lambda (s)
    (cdr (force s))))

(define counters
  (let next ([n 1])
    (delay (cons n (next (+ n 1))))))

(stream-car counters) → 1

(stream-car (stream-cdr counters)) → 2

(define stream-add
  (lambda (s1 s2)
    (delay (cons
             (+ (stream-car s1) (stream-car s2))
             (stream-add (stream-cdr s1) (stream-cdr s2))))))

(define even-counters
  (stream-add counters counters))

(stream-car even-counters) → 2

(stream-car (stream-cdr even-counters)) → 4
#|-delayed-evaluation-1|#

(define-syntax delay
  (syntax-rules ()
    [(_ expr) (make-promise (lambda () expr))]))

(define make-promise
  (lambda (p)
    (let ([val #f] [set? #f])
      (lambda ()
        (unless set?
          (let ([x (p)])
            (unless set?
              (set! val x)
              (set! set? #t))))
        val))))

(define force
  (lambda (promise)
    (promise)))

#|+delayed-evaluation-2|#
(define make-promise
  (lambda (p)
    (let ([vals #f] [set? #f])
      (lambda ()
        (unless set?
          (call-with-values p
            (lambda x
              (unless set?
                (set! vals x)
                (set! set? #t)))))
        (apply values vals)))))

(define p (delay (values 1 2 3)))
(force p) → 1
            2
            3
(call-with-values (lambda () (force p)) +) → 6
#|-delayed-evaluation-2|#

#|+delayed-evaluation-3|#
(define-record-type promise
  (fields (immutable p) (mutable vals) (mutable set?))
  (protocol (lambda (new) (lambda (p) (new p #f #f)))))

(define force
  (lambda (promise)
    (unless (promise? promise)
      (assertion-violation 'promise "invalid argument" promise))
    (unless (promise-set? promise)
      (call-with-values (promise-p promise)
        (lambda x
          (unless (promise-set? promise)
            (promise-vals-set! promise x)
            (promise-set?-set! promise #t)))))
    (apply values (promise-vals promise))))
#|-delayed-evaluation-3|#

#|+multiple values-1|#
(values) →

(values 1) → 1

(values 1 2 3) → 1
                 2
                 3

(define head&tail
  (lambda (ls)
    (values (car ls) (cdr ls))))

(head&tail '(a b c)) → a
                      (b c)
#|-multiple values-1|#

#|+multiple values-2|#
(call-with-values
  (lambda () (values 'bond 'james))
  (lambda (x y) (cons y x))) → (james . bond)

(call-with-values values list) → '()
#|-multiple values-2|#

#|+multiple values-3|#
(define dxdy
  (lambda (p1 p2)
    (values (- (car p2) (car p1))
            (- (cdr p2) (cdr p1)))))

(dxdy '(0 . 0) '(0 . 5)) → 0
                          5
#|-multiple values-3|#

#|+multiple values-4|#
(define segment-length
  (lambda (p1 p2)
    (call-with-values
      (lambda () (dxdy p1 p2))
      (lambda (dx dy) (sqrt (+ (* dx dx) (* dy dy)))))))

(define segment-slope
  (lambda (p1 p2)
    (call-with-values
      (lambda () (dxdy p1 p2))
      (lambda (dx dy) (/ dy dx)))))

(segment-length '(1 . 4) '(4 . 8)) → 5
(segment-slope '(1 . 4) '(4 . 8)) → 4/3
#|-multiple values-4|#

#|+multiple values-5|#
(define describe-segment
  (lambda (p1 p2)
    (call-with-values
      (lambda () (dxdy p1 p2))
      (lambda (dx dy)
        (values
          (sqrt (+ (* dx dx) (* dy dy)))
          (/ dy dx))))))

(describe-segment '(1 . 4) '(4 . 8)) → 5
                                     → 4/3
#|-multiple values-5|#

#|+multiple values-6|#
(define split
  (lambda (ls)
    (if (or (null? ls) (null? (cdr ls)))
        (values ls '())
        (call-with-values
          (lambda () (split (cddr ls)))
          (lambda (odds evens)
            (values (cons (car ls) odds)
                    (cons (cadr ls) evens)))))))

(split '(a b c d e f)) → (a c e)
                         (b d f)
#|-multiple values-6|#

#|+multiple values-7|#
(+ (values 2) 4) → 6

(if (values #t) 1 2) → 1

(call-with-values
  (lambda () 4)
  (lambda (x) x)) → 4
#|-multiple values-7|#

#|+multiple values-8|#
(call-with-values
  (lambda ()
    (call/cc (lambda (k) (k 2 3))))
  (lambda (x y) (list x y))) → (2 3)
#|-multiple values-8|#

#|+multiple values-9|#
(if (values 1 2) 'x 'y)

(+ (values) 5)
#|-multiple values-9|#

#|+multiple values-10|#
(define-syntax first
  (syntax-rules ()
    [(_ expr)
     (call-with-values
       (lambda () expr)
       (lambda (x . y) x))]))

(if (first (values #t #f)) 'a 'b) → a
#|-multiple values-10|#

#|+multiple values-11|#
(call-with-values
  (lambda () (values 2 3 4))
  (lambda (x y) x))

(call-with-values
  (lambda () (call/cc (lambda (k) (k 0))))
  (lambda (x y) x))
#|-multiple values-11|#

#|+multiple values-12|#
(define-syntax with-values
  (syntax-rules ()
    [(_ expr consumer)
     (call-with-values (lambda () expr) consumer)]))

(with-values (values 1 2) list) → (1 2)
(with-values (split '(1 2 3 4))
  (lambda (odds evens)
    evens)) → (2 4)
#|-multiple values-12|#

#|+multiple values-13|#
(let-values ([(odds evens) (split '(1 2 3 4))])
  evens) → (2 4)

(let-values ([ls (values 'a 'b 'c)])
  ls) → (a b c)
#|-multiple values-13|#

(define call-with-port
  (lambda (port proc)
    (let-values ([val* (proc port)])
      (close-port port)
      (apply values val*))))

#|+multiple values-14|#
(define call-with-port
  (lambda (port proc)
    (call-with-values (lambda () (proc port))
      (case-lambda
        [(val) (close-port port) val]
        [val* (close-port port) (apply values val*)]))))
#|-multiple values-14|#

#|+multiple values-15|#
(library (mrvs)
  (export call-with-values values call/cc
    (rename (call/cc call-with-current-continuation)))
  (import
    (rename
      (except (rnrs) values call-with-values)
      (call/cc rnrs:call/cc)))

  (define magic (cons 'multiple 'values))

  (define magic?
    (lambda (x)
      (and (pair? x) (eq? (car x) magic))))

  (define call/cc
    (lambda (p)
      (rnrs:call/cc
        (lambda (k)
          (p (lambda args
               (k (apply values args))))))))

  (define values
    (lambda args
      (if (and (not (null? args)) (null? (cdr args)))
          (car args)
          (cons magic args))))

  (define call-with-values
    (lambda (producer consumer)
      (let ([x (producer)])
        (if (magic? x)
            (apply consumer (cdr x))
            (consumer x))))))
#|-multiple values-15|#

#|+eval-1|#
(define cons 'not-cons)
(eval '(let ([x 3]) (cons x 4)) (environment '(rnrs))) → (3 . 4)

(define lambda 'not-lambda)
(eval '(lambda (x) x) (environment '(rnrs))) → #<procedure>

(eval '(cons 3 4) (environment)) → exception
#|-eval-1|#

#|+eval-2|#
(define env (environment '(rnrs) '(prefix (rnrs lists) $)))
(eval '($cons* 3 4 (* 5 8)) env) → (3 4 . 40)
#|-eval-2|#
