;;; -*- lexical-binding: t; -*-

;;ex1.6
(defun square (x)
  (* x x))

(defun is-good-enough? (x guess)
  (< (abs (- (square guess) x))
     0.001))

(defun average (x y)
  (/ (+ x y) 2))

(defun improve-guess (x guess)
  (average guess (/ x guess)))

(defun sqrt-iter (x guess)
  (if (is-good-enough? x guess)
      guess
    (sqrt-iter x (improve-guess x guess))))

(defun sqrt (x)
  (sqrt-iter x 1.0))

;;(sqrt 2)

;;ex1.7
(defun is-good-enough-v2? (next-guess guess)
  (< (/ (abs (- next-guess guess))
        guess)
     0.0001))

(defun sqrt-iter-v2 (x guess)
  (let ((next-guess (improve-guess x guess)))
    (if (is-good-enough-v2? next-guess guess)
        guess
      (sqrt-iter-v2 x next-guess))))

(sqrt-iter-v2 0.01 1.0)

;;ex1.8
(defun cube (x)
  (* x x x))

(defun is-good-enough-cube-root? (x guess)
  (< (abs (- (cube guess) x)) 0.001))

(defun improve-cube-guess (x guess)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3.0))

(defun cubert-iter (x guess)
  (if (is-good-enough-cube-root? x guess)
      guess
    (cubert-iter x (improve-cube-guess x guess))))

;;(cubert-iter 8 1.0)

(defun cube-root (x)
  (cubert-iter x 1.0))

(cube-root 8)

;;ex1.9

(defun inc (x)
  (+ x 1))

(defun dec (x)
  (- x 1))

(defun plus-recur (a b)
  (if (= a 0)
      b
    (inc (plus-recur (dec a) b))))

(defun plus-iter (a b)
  (if (= a 0)
      b
    (plus-iter (dec a) (inc b))))

;;ex1.10
;; Ackermann's function
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1)
              (A x (- y 1))))))

;;(A 1 10) -> 1024
;;(A 2 4) -> 65536
;;(A 3 3) -> 65536

(defun f (n)
  (A 0 n))
(defun g (n)
  (A 1 n))
(defun h (n)
  (A 2 n))

;;(mapcar 'f '(1 2 3 4)) -> (2 4 6 8) 2n
;;(mapcar 'g '(1 2 3 4)) -> (2 4 8 16) 2^n
;;(mapcar 'h '(0 1 2 3 4)) -> (0 2 4 16 65536) ???

;;Ex: Counting Change

(defun first-denomination (kind-of-coins)
  (nth kind-of-coins '(0 1 5 10 25 50)))

(defun cc (amount kind-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kind-of-coins 0)) 0)
        (t (+ (cc amount (- kind-of-coins 1))
              (cc (- amount (first-denomination kind-of-coins)) kind-of-coins)))))

(defun count-change (amount)
  (cc amount 5))

;;(count-change 100) -> 292

;;ex1.11

(defun ex11-recur (n)
  (cond ((<= n 3) n)
        (t (+ (ex11-recur (- n 1))
              (* 2 (ex11-recur (- n 2)))
              (* 3 (ex11-recur (- n 3)))))))

;;(ex11-recur 10)

(defun ex11-iter (n)
  (let (value)
    (dotimes (i (+ n 1) value)
      (setq value (cons (cond
                         ((<= i 3) i)
                         (t (+ (nth 0 value)
                               (* 2 (nth 1 value))
                               (* 3 (nth 2 value))))) value)))
    (car value)))

;;(ex11-iter 10)

;;ex1.12

(defun pascal (row col)
  (cond ((or (< row 0)
             (< col 0)) 0)
        ((and (= row 0) (= col 0)) 1)
        (t (+ (pascal (- row 1) (- col 1))
              (pascal (- row 1) col)))))

;;(pascal 5 3) -> 10

;;ex1.16
(defun expt (b n)
  (if (= n 0)
      1
    (* b (expt b (- n 1)))))

;;(expt 2 5)

(defun expt-iter (b n acc)
  (if (= n 0)
      acc
    (expt-iter b (- n 1) (* b acc))))

;;(expt-iter 2 5 1)
(defun evenp (n)
  (= (mod n 2) 0))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

;;(fast-expt 2 40)

(defun fast-expt-iter (b n acc)
  (cond ((= n 0) acc)
        ((evenp n) (fast-expt-iter (* b b) (/ n 2) acc))
        (t (fast-expt-iter b (- n 1) (* b acc)))))

;;(fast-expt-iter 2 40 1)

;;ex1.17
(defun multiply (a b)
  (if (= b 0)
      0
    (+ a (multiply a (- b 1)))))

;;(multiply 7 8)

(defun double (n)
  (+ n n))

(defun half (n)
  (/ n 2))

(defun fast-multiply (a b)
  (cond ((= b 0) 0)
        ((evenp b) (fast-multiply (double a) (half b)))
        (t (+ a (fast-multiply a (- b 1))))))
;;(fast-multiply 7 8)

;;ex1.18
(defun fast-multiply-iter (a b acc)
  (cond ((= b 0) acc)
        ((evenp b) (fast-multiply-iter (double a) (half b) acc))
        (t (fast-multiply-iter a (- b 1) (+ a acc)))))
;;(fast-multiply-iter 7 8 0)

;;ex1.19

(defun fast-fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((evenp count) (fast-fib-iter a
                                      b
                                      (+ (square p) (square q))
                                      (+ (square q) (* 2 p q))
                                      (/ count 2)))
        (t (fast-fib-iter (+ (* q a) (* q b) (* p a))
                          (+ (* q a) (* p b))
                          p
                          q
                          (- count 1)))))
(defun fast-fib (n)
  (fast-fib-iter 1 0 0 1 n))

;;(fast-fib 10)

;;ex 1.21

(defun divides? (a b)
  (= (mod a b) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (t (find-divisor n (+ 1 test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

;;(smallest-divisor 19999)

(defun prime? (n)
  (= n (smallest-divisor n)))

;; ex1.22

(defmacro with-time (&rest body)
  `(let* ((time (current-time))
          (result ,@body)
          (x (make-hash-table)))
     (puthash :result result x)
     (puthash :time (format "%.06f" (float-time (time-since time))) x)
     x))

(with-time (prime? 100003))

(defun search-for-primes (n count primes-list)
  (if(= count 0)
      primes-list
    (let ((res-map (with-time (prime? n))))
      (if (gethash :result res-map)
          (search-for-primes (+ n 1) (- count 1) (cons (list n (gethash :time res-map)) primes-list))
        (search-for-primes (+ n 1) count primes-list)))))

;;(search-for-primes 1000 3 '()) -> ((1019 "0.000213") (1013 "0.000214") (1009 "0.000216"))
;;(search-for-primes 10000 3 '()) -> ((10037 "0.000661") (10009 "0.000752") (10007 "0.000674"))
;; (* 0.000213 (sqrt 10)) -> 0.000673

;;ex 1.23

(defun next (n)
  (if (= n 2)
      3
    (+ 2 n)))

(defun find-divisor2 (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (t (find-divisor2 n (next test-divisor)))))

(defun smallest-divisor2 (n)
  (find-divisor2 n 2))

;;(smallest-divisor 19999)

(defun prime2? (n)
  (= n (smallest-divisor2 n)))

(defun search-for-primes2 (n count primes-list)
  (if(= count 0)
      primes-list
    (let ((res-map (with-time (prime2? n))))
      (if (gethash :result res-map)
          (search-for-primes2 (+ n 1) (- count 1) (cons (list n (gethash :time res-map)) primes-list))
        (search-for-primes2 (+ n 1) count primes-list)))))

;;(search-for-primes 10000 3 '()) -> ((10037 "0.000661") (10009 "0.000752") (10007 "0.000674"))
;;(search-for-primes2 10000 3 '()) -> ((10037 "0.000457") (10009 "0.000430") (10007 "0.000441"))

;;ex1.24

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp) (mod (square (expmod base (/ exp 2) m))
                          m))
        (t (mod (* base (expmod base (- exp 1) m))
                m))))

(defun fermat-test (n)
  (let ((rand-int (+ 1 (random (- n 1)))))
    (= (expmod rand-int n n) rand-int)))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

;;(fast-prime? 10037 5)
;;(with-time (fast-prime? 1009 10)) -> (:result t :time "0.001221")
;;(with-time (fast-prime? 1000033 10)) -> (:result t :time "0.002006")

;;ex1.27

;;Carmichael numbers -> 561, 1105, 1729, 2465, 2821, 6601

(defun fermat-test-all (n x)
  (cond ((= x 0) t)
        ((= (expmod x n n) x) (fermat-test-all n (- x 1)))
        (t nil)))

;;(let ((max-lisp-eval-depth 400000)) (fermat-test-all 561 560)) -> t

;;ex1.28
(defun miller-rabin-expmod (base exp m)
  (let ((squaremod-with-check (function (lambda (x)
                                          (let ((square (mod (square x) m)))
                                            (if  (and (= square 1)
                                                      (not (= x 1))
                                                      (not (= x (- m 1))))
                                                0
                                              square))))))
    (cond ((= exp 0) 1)
          ((evenp exp) (funcall squaremod-with-check (miller-rabin-expmod base (/ exp 2) m)))
          (t (mod (* base (miller-rabin-expmod base (- exp 1) m))
                  m)))))

(defun miller-rabin-fermat-test-all (n x)
  (cond ((= x 0) t)
        ((= (miller-rabin-expmod x n n) x) (miller-rabin-fermat-test-all n (- x 1)))
        (t nil)))

;;(miller-rabin-fermat-test-all 37 36) -> t
;;(let ((max-lisp-eval-depth 400000)) (miller-rabin-fermat-test-all 561 560)) -> nil

(defun sum (term a next b)
  (if (> a b)
      0
    (+ (funcall term a)
       (sum term (funcall next a) next b))))

(defun sum-cubes (a b)
  (sum (lambda (x) (* x x x)) a 'inc b))

;;(sum-cubes 1 10)

(defun pi-sum (a b)
  (let ((pi-term  (lambda (x)
                    (/ 1.0 (* x (+ x 2)))))
        (pi-next (lambda (x)
                   (+ x 4))))
    (sum pi-term a pi-next b)))

;;(* 8 (pi-sum 1 1000))

(defun integral (f a b dx)
  (let ((add-dx (lambda (x) (+ x dx))))
    (* (sum f (+ a (/ dx 2)) add-dx b )
       dx)))

(defun cube (x) (* x x x))

;;(integral 'cube 0 1 0.01) -> 0.24998750000000042 

;;ex1.29

(defun simpson (f a b n)
  (let* ((h (/ (- b a) n))
         (simpson-term (lambda (k)
                         (* (cond ((or (= k 0) (= k n)) 1)
                                  ((evenp k) 2)
                                  (t 4))
                            (funcall f (+ a (* k h))))))
         (simpson-next (lambda (x) (+ x 1))))
    (* (/ h 3)
       (sum simpson-term 0 simpson-next 100))))

;;(simpson 'cube 0 1.0 100) -> 0.24999999999999992

;;ex1.30
;; Use https://github.com/Wilfred/tco.el for tail call optimization

'(defun sum-iter (term a next b)
   (labels ((iter (x result)
                  (if (> x b)
                      result
                    (iter (funcall next x) (+ result (funcall term x))))))
     (iter a 0)))

;;(sum-iter 'cube 1 'inc 10)

;;ex1.31

(defun product (term a next b)
  (if (> a b)
      1
    (* (funcall term a)
       (product term (funcall next a) next b))))

(defun identity (x) x)
(defun factorial (n)
  (product 'identity 1 'inc n))
;;(factorial 4)

(defun wallis-product (n)
  (let ((term (lambda (x) (if (evenp x)
                              (/ (+ x 2) (+ x 1))
                            (/ (+ x 1) (+ x 2))))))
    (product term 1.0 'inc n)))
;;(* 4 (wallis-product 100)) -> 3.1570301764551645

;;ex1.32

(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
    (funcall combiner
             (funcall term a)
             (accumulate combiner null-value term (funcall next a) next b))))

(defun sum2 (term a next b)
  (accumulate '+ 0 term a next b))
;;(sum2 'identity 0 'inc 10)

(defun product2 (term a next b)
  (accumulate '* 1 term a next b))
;;(product 'identity 1  'inc 10)

;; ex1.33
(defun filtered-accumulate (filter? combiner null-value term a next b)
  (cond
   ((> a b) null-value)
   ((funcall filter? a) (funcall combiner
                                 (funcall term a)
                                 (filtered-accumulate filter? combiner null-value term (funcall next a) next b)))
   (t (filtered-accumulate filter? combiner null-value term (funcall next a) next b))))

(defun sum-of-primes (a b)
  (filtered-accumulate 'prime2? '+ 0 'identity a 'inc b))

;;(sum-of-primes 10 20) -> 60(11,13,17,19)

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (mod a b))))

(defun relative-prime-product (n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1)) '* 1 'identity 1 'inc (- n 1)))

;;(relative-prime-product 8) -> 105 (3,5,7)

;; half-interval method

(defun positive? (n)
  (> n 0))

(defun negative? (n)
  (< n 0))
(defun close-enough? (a b)
  (< (abs (- a b)) 0.001))

(defun search (f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
      (let ((test-value (funcall f mid-point)))
        (cond ((positive? test-value) (search f neg-point mid-point))
              ((negative? test-value) (search f mid-point pos-point))
              (:else mid-point))))))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (:else "Values are not of opposing sign"))))

;;(half-interval-method 'sin 2.0 4.0) -> 3.14111328125
;;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0) -> 1.89306640625

;;Fixed point of functions

(defun close-enough-with-tolerance? (a b tolerance)
  (< (abs (- a b)) tolerance))

(defun fixed-point (f guess)
  (let ((next (funcall f guess)))
    (if (close-enough-with-tolerance? next guess 0.001)
        next
      (fixed-point f next))))

;;(fixed-point 'cos 1.0) -> 0.7387603198742113
;;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) -> 1.259003859740025

(defun sqrt-fix (x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
;;(sqrt-fix 2) -> 1.4142135623746899

;;ex1.35
(defconst phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0) "golden ratio")
;; phi -> 1.6184321884964956

;;ex1.36

(defun fixed-point-with-print (f guess)
  (let ((next (funcall f guess)))
    (print next)
    (if (close-enough-with-tolerance? next guess 0.001)
        next
      (fixed-point-with-print f next))))

;;(fixed-point-with-print (lambda (x) (/ (log 1000) (log x))) 2.0) -> 23 steps
;;(fixed-point-with-print (lambda (x) (average x (/ (log 1000) (log x)))) 2.0) -> 6 steps

;;ex1.37

(defun cont-frac (n d k)
  (letrec ((iter (lambda (i result)
                   (if (= i 0)
                       result
                     (funcall iter (- i 1) (/ (funcall n i) (+ (funcall d i) result)))))))
    (funcall iter k 0)))

;;(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 10)

(let* ((k 1)
      (res (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) k)))
  (while (not (close-enough-with-tolerance? (/ 1 phi) res 0.0001))
    (setq k (+ k 1))
    (setq res (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) k)))
  (list k res))
;;(10 0.6179775280898876)

;;ex1.38
(defun e-euler-den (n)
  (if (= (mod n 3) 2)
      (* 2 (/ (+ n 1) 3.0))
    1))

(defun e-euler (k)
  (+ 2
     (cont-frac (lambda (x) 1.0) 'e-euler-den k)))

;;(e-euler 10) -> 2.7182817182817183

;;ex1.39

(defun tan-cf (x k)
  (cont-frac (lambda (i) (if (= i 1) x (* -1 x x)))
             (lambda (i) (- (* 2 i) 1))
             k))

;;(- (tan-cf 1.5 10) (tan 1.5))

;; Procedures as return values

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))

;;(funcall (average-damp 'square) 2)

(defun sqrt-fix2 (x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
;;(sqrt-fix2 2) -> 1.4142135623746899

;; Newton's method

(defconst dx 0.00001)
(defun deriv (f)
  (lambda (x) (/ (- (funcall f (+ x dx))
                    (funcall f x))
                 dx)))

;;(funcall (deriv 'cube) 5)

(defun newton-transform (f)
  (lambda (x) (- x (/ (funcall f x) (funcall (deriv f) x)))))

(defun newtons-method (f guess)
  (fixed-point (newton-transform f) guess))

(defun sqrt-newton (x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

;;(sqrt-newton 2) -> 1.4142135623822438

(defun fixed-point-of-transform (f transform guess)
  (fixed-point (funcall transform f) guess))

(defun sqrt-damp-transform (x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            'average-damp
                            1.0))

;;(sqrt-damp-transform 2) -> 1.4142135623746899

(defun sqrt-newton-transform (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            'newton-transform
                            1.0))

;;(sqrt-newton-transform 2) -> 1.4142135623822438

;;ex1.40

(defun cubic (a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;;(newtons-method (cubic -1 -3 2) 4.0) -> 2.000000002519986

;;ex1.41

(defun double (f)
  (lambda (x) (funcall f (funcall f x))))

;;(funcall (funcall (double (double 'double)) 'inc) 5) -> 21

;;ex1.42

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;;(funcall (compose 'square 'inc) 6) -> 49

;;ex1.43

(defun repeated (f n)
  (if (= n 1)
      f
    (compose f (repeated f (- n 1)))))

;;(funcall (repeated 'square 2) 5) -> 625

;;ex1.44

(defun smooth (f)
  (lambda (x) (/ (+ (funcall (+ x dx))
                    (funcall f x)
                    (funcall (- x dx)))
                 3)))

(defun n-fold-smooth (f n)
  (funcall (repeated 'smooth n) f))

;;ex1.45

(defun nth-root (x n)
  (fixed-point (funcall (repeated 'average-damp (floor (log n 2)))
                        (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;;(nth-root 1024 10) -> 1.9999243396513986

;;ex1.46

(defun iterative-improve (good-enough? f)
  (letrec ((iter (lambda (x) (let ((next (funcall f x)))
                               (if (funcall good-enough? x next)
                                   next
                                 (funcall iter next))))))
    (lambda (guess)
      (funcall iter guess))))

(defun sqrt-iter-improve (n)
  (funcall (iterative-improve 'close-enough? (average-damp (lambda (y) (/ n y)))) 1.0))

;;(sqrt-iter-improve 2) -> 1.4142135623746899

(defun fixed-point-iter-improve (f guess)
  (funcall (iterative-improve 'close-enough? f) guess))

;;(fixed-point-iter-improve 'cos 1.0) -> 0.7387603198742113
