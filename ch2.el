;;; -*- lexical-binding: t; -*-

(defun make-rat (n d)
  (cons n d))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (message "%d/%d" (numer x) (denom x)))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;;(defconst one-half (make-rat 1 2))
;;(defconst one-third (make-rat 1 3))
;;(print-rat one-half)
;;(print-rat (add-rat one-half one-third))
;;(print-rat (sub-rat one-half one-third))
;;(print-rat (mul-rat one-half one-third))

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (mod a b))))

(defun make-rat2 (x y)
  (let ((g (gcd x y)))
    (cons (/ x g) (/ y g))))

;;(make-rat2 4 10) -> (2 . 5)

;;ex2.1

(defun make-rat3 (x y)
  (let ((g (* (if (< y 0) -1 1)
              (abs (gcd x y)))))
    (cons (/ x g) (/ y g))))

;;(make-rat3 2 -4)

;;ex2.2

(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun make-segment (p1 p2)
  (cons p1 p2))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

(defun average (a b)
  (/ (+ a b) 2))

(defun midpoint-segment (s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(defun print-point (p)
  (message "(%d,%d)" (x-point p) (y-point p)))

;; (print-point (midpoint-segment (make-segment (make-point 10 1) (make-point 2 3))))

;;ex2.3
(defun make-rect (p1 p2)
  (cons p1 p2))

(defun rect-length (r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (- (x-point p1) (x-point p2)))))

(defun rect-height (r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (- (y-point p1) (y-point p2)))))

(defun rect-perimeter (r)
  (* 2 (+ (rect-length r) (rect-height r))))

(defun rect-area (r)
  (* (rect-length r) (rect-height r)))

;;(rect-perimeter (make-rect (make-point 10 1) (make-point 2 3)))
;;(rect-area (make-rect (make-point 10 1) (make-point 2 3)))

(defun m-cons (x y)
  (let ((dispatch (lambda (m)
                    (cond ((= m 0) x)
                          ((= m 1) y)
                          (:else (error "Argument not 0 or 1 -- CONS %d" m))))))
    dispatch))

(defun m-car (z)
  (funcall z 0))

(defun m-cdr (z)
  (funcall z 1))

;;(m-cdr (m-cons 3 6))
;;(funcall (m-cons 0 1) 2)

;;ex2.4

(defun m2-cons (x y)
  (lambda (m) (funcall m x y)))

(defun m2-car (z)
  (funcall z (lambda (p q) p)))

(defun m2-cdr (z)
  (funcall z (lambda (p q) q)))

;;(m2-car (m2-cons 1 2))
;;(m2-cdr (m2-cons 1 2))

;;ex2.5

(defun m3-cons (x y)
  (* (expt 2 x) (expt 3 y)))

(defun largest-power-of (a z)
  (letrec ((iter (lambda (res z)
                   (if (= 0 (mod z a))
                       (funcall iter (+ 1 res) (/ z a))
                     res))))
    (funcall iter 0 z)))

(defun m3-car (z)
  (largest-power-of 2 z))

(defun m3-cdr (z)
  (largest-power-of 3 z))

;;(m3-car 1024)
;;(m3-cdr (m3-cons 2 3))

;;ex2.6

(defconst zero (lambda (f) (lambda (x) x)))
(defconst one (lambda (f) (lambda (x) (funcall f x))))
(defconst two (lambda (f) (lambda (x) (funcall f (funcall f x)))))

;;(defun add (m n) (lambda (f) (lambda (x) (funcall (m f) (funcall (m f) x)))))

;;ex2.7

(defun make-interval (a b)
  (if (< a b)
      (cons a b)
    (cons b a)))

(defun lower-bound (x)
  (car x))

(defun upper-bound (x)
  (cdr x))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;ex2.8

(defun sub-interval (x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;;ex2.9

(defun width-interval (x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;;(width-interval (make-interval 1.5 2.5))
;;(width-interval (make-interval 3 7))
;;(width-interval (add-interval (make-interval 1.5 2.5) (make-interval 3 7)))

;;ex2.10

(defun div-interval2 (x y)
  (if (>= 0 (* (upper-bound y) (lower-bound y)))
      (error "Interval spans zero")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;;(div-interval2 (make-interval 1 2) (make-interval 3 -4))

;;ex2.11

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun center (i)
  (average (lower-bound i) (upper-bound i)))

(defun width (i)
  (/ (- (lower-bound i) (upper-bound i)) 2))

;;ex2.12

(defun make-interval-center-percent (c p)
  (let ((w (/ (* c p) 100)))
    (make-interval (- c w) (+ c w))))

(defun percent (i)
  (* (/ (width i) (center i)) 100))

;;(make-interval-center-percent 10 50)

;;2.2.1

(defun m-list-ref (items n)
  (if (= n 0)
      (car items)
    (m-list-ref (cdr items) (- n 1))))

;;(m-list-ref (list 1 2 3 4 5) 3)

(defun m-length (items)
  (if (null items)
      0
    (+ 1 (m-length (cdr items)))))

;;(m-length (list 1 2 3 4))

(defun length-iter (items)
  (letrec ((iter (lambda (a count)
                   (if (null a)
                       count
                     (funcall iter (cdr a) (+ 1 count))))))
    (funcall iter items 0)))

;;(length-iter (list 1 2 3 4))

(defun m-append (list1 list2)
  (if (null list1)
      list2
    (cons (car list1)
          (m-append (cdr list1) list2))))

;; (m-append (list 1 3 5) (list 2 4 6)) -> (1 3 5 2 4 6)

;;ex2.17

(defun last-pair (items)
  (let ((rest (cdr items)))
    (if (null rest)
        items
      (last-pair rest))))

;;(last-pair '(1 2 3 4))

;;ex2.18

(defun m-reverse (items)
  (letrec ((iter (lambda (items acc)
                   (if (null items)
                       acc
                     (funcall iter (cdr items) (cons (car items) acc))))))
    (funcall iter items nil)))

;; (m-reverse '(1 2 3 4))

;;ex2.19

(defconst us-coins (list 50 25 10 5 1))
(defconst uk-coins (list 100 50 20 10 5 2 1 0.5))

(defun no-more? (items)
  (= 0 (length items)))

(defun first-denomination (items)
  (car items))

(defun except-first-denomination (items)
  (cdr items))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (t (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values))
                  coin-values)))))
;;(cc 100 us-coins) -> 292
;;(cc 100 uk-coins) -> 104561

;;ex2.20

(defun evenp (x)
  (= 0 (mod x 2)))

(defun same-parity (x &rest items)
  (letrec ((rem (mod x 2))
           (iter (lambda (items acc)
                   (cond ((null items) acc)
                         ((= rem (mod (car items) 2))
                          (funcall iter (cdr items) (cons (car items) acc)))
                         (t (funcall iter (cdr items) acc))))))
    (reverse (funcall iter items nil))))

;;(same-parity 1 2 3 4 5)

(defun m-map (proc items)
  (if (null items)
      nil
    (cons (funcall proc (car items))
          (m-map proc (cdr items)))))

(defun scale-list (items factor)
  (m-map (lambda (x) (* x factor)) items))

;;(scale-list '(1 2 3 4) 10) -> (10 20 30 40)

;;ex2.21

(defun square-list (items)
  (if (null items)
      nil
    (cons (expt (car items) 2) (square-list (cdr items)))))

(defun square-list-map (items)
  (m-map (lambda (x) (* x x)) items))

;;(square-list (list 1 2 3 4))
;;(square-list-map (list 1 2 3 4))

;;ex2.23

(defun for-each (proc items)
  (if (null items)
      t
    (progn
      (funcall proc (car items))
      (for-each proc (cdr items)))))

;;(for-each (lambda (x) (print x)) '(1 2 3 4))

(defun count-leaves (x)
  (cond ((null x) 0)
        ((not (listp x)) 1)
        (t (+ (count-leaves (car x))
              (count-leaves (cdr x))))))

;;(count-leaves (list 1 2 (list 3 4 (list 5 6))))

;;ex2.27

(defun m-deep-reverse (items)
  (letrec ((iter (lambda (items acc)
                   (cond ((null items) acc)
                         ((not (listp items)) items)
                         (t (funcall iter (cdr items)
                                     (cons (m-deep-reverse (car items)) acc)))))))
    (funcall iter items nil)))

;;(m-deep-reverse (list 1 (list 2 (list 3 4) 5) 6)) -> (6 (5 (4 3) 2) 1)


;;ex2.28

(defun fringe (items)
  (letrec ((iter (lambda (items acc)
                   (let ((first (car items))
                         (rest (cdr items)))
                     (cond
                      ((null items) acc)
                      ((not (listp first)) (cons first
                                                 (funcall iter rest acc)))
                      (:else (funcall iter first
                                      (funcall iter rest acc))))))))
    (funcall iter items nil)))

(defconst xa (list 1 2 (list 3 (list (list 4 5) 6))))

;; (fringe xa) -> (1 2 3 4 5 6)

;;ex2.29

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (car (cdr mobile)))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (car (cdr branch)))

(defun total-weight (mobile)
  (cond ((null mobile) 0)
        ((not (listp mobile)) mobile)
        (:else (+ (total-weight (branch-structure (left-branch mobile)))
                  (total-weight (branch-structure (right-branch mobile)))))))

(defconst a (make-mobile (make-branch 2 3) (make-branch 3 2)))

;;(total-weight a) -> 5

(defun torque (branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(defun balanced? (mobile)
  (if (not (listp mobile))
      t
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile))))))

(defconst d (make-mobile (make-branch 10 a) (make-branch 25 2)))
;; ((10 ((2 3) (3 2))) (25 2)) 
;;(balanced? d) -> t


(defun scale-tree (tree factor)
  (cond ((null tree) nil)
        ((not (listp tree)) (* tree factor))
        (:else (cons (scale-tree (car tree) factor)
                     (scale-tree (cdr tree) factor)))))

;;(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10) -> (10 (20 (30 40) 50) (60 70))

(defun scale-tree2 (tree factor)
  (m-map (lambda (subtree)
           (if (listp subtree)
               (scale-tree2 subtree factor)
             (* subtree factor))) tree))

;;(scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10) -> (10 (20 (30 40) 50) (60 70))

;;ex2.30

(defun square (x) (* x x))
(defun square-tree (tree)
  (cond ((null tree) nil)
        ((not (listp tree)) (square tree))
        (:else (cons (square-tree (car tree))
                     (square-tree (cdr tree))))))

;;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) -> (1 (4 (9 16) 25) (36 49))

(defun square-tree-map (tree)
  (m-map (lambda (subtree)
           (if (listp subtree)
               (square-tree-map subtree)
             (square subtree))) tree))

;;(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7))) -> (1 (4 (9 16) 25) (36 49))

;;ex2.31

(defun tree-map (f tree)
  (m-map (lambda (subtree)
           (if (listp subtree)
               (tree-map f subtree)
             (funcall f subtree))) tree))

(defun square-tree2 (tree)
  (tree-map 'square tree))

;;(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7))) -> (1 (4 (9 16) 25) (36 49))

;;ex2.32

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (m-append rest
                (m-map (lambda (x) (cons (car s) x)) rest)))))

;;(subsets (list 1 2 3)) -> (nil (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
        ((not (listp tree)) (if (oddp tree) (square tree) 0))
        (:else (+ (sum-odd-squares (car tree))
                  (sum-odd-squares (cdr tree))))))

;;(sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (:else (+ (fib (- n 1))
                  (fib (- n 2))))))


(defun even-fibs (n)
  (letrec ((next (lambda (k)
                   (cond ((> k n) nil)
                         ((evenp (fib k)) (cons (fib k)
                                                (funcall next (+ k 1))))
                         (:else (funcall next (+ k 1)))))))
    (funcall next 0)))

;;(even-fibs 10) -> (0 2 8 34)

(defun m-filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence)) (cons (car sequence)
                                                  (m-filter predicate (cdr sequence))))
        (:else (m-filter predicate (cdr sequence)))))

;;(m-filter 'oddp (list 1 2 3 4 5 6 7 8 9)) -> (1 3 5 7 9)

(defun m-accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (m-accumulate op initial (cdr sequence)))))

;;(m-accumulate '* 1 (list 1 2 3 4 5)) -> 120
(defun enumerate-interval (low high)
  (if (> low high)
      nil
    (cons low (enumerate-interval (+ low 1) high))))

;;(enumerate-interval 3 10) -> (3 4 5 6 7 8 9 10)

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
        ((not (listp tree)) (list tree))
        (:else (m-append (enumerate-tree (car tree))
                         (enumerate-tree (cdr tree))))))

;;(enumerate-tree (list 1 (list 2 (list 3 4)) 5)) -> (1 2 3 4 5)

(defun sum-odd-squares2 (tree)
  (m-accumulate '+ 0
                (m-map 'square
                       (m-filter 'oddp
                                 (enumerate-tree tree)))))

;;(sum-odd-squares2 (list 1 (list 2 (list 3 4) 5) (list 6 7))) -> 84

(defun even-fibs2 (n)
  (m-filter 'evenp
            (m-map 'fib
                   (enumerate-interval 0 n))))
;;(even-fibs2 10) -> (0 2 8 34)

(defun list-fib-squares (n)
  (m-map 'square
         (m-map 'fib
                (enumerate-interval 0 n))))

;;(list-fib-squares 10) -> (0 1 1 4 9 25 64 169 441 1156 3025)

(defun product-of-squares-of-odd-elements (sequence)
  (square (m-accumulate '* 1
                        (m-filter 'oddp
                                  sequence))))

;;(product-of-squares-of-odd-elements (list 1 2 3 4 5)) -> 225

;;ex 2.33

(defun m-map2 (p sequence)
  (m-accumulate (lambda (x y) (cons (funcall p x) y)) nil sequence))

;;(m-map2 'square (list 1 2 3 4 5)) -> (1 4 9 16 25)

(defun m-append2 (seq1 seq2)
  (m-accumulate (lambda (x y) (cons x y)) seq2 seq1))

;;(m-append2 (list 1 2 3) (list 4 5 6))

(defun length2 (sequence)
  (m-accumulate (lambda (_ y) (+ 1 y)) 0 sequence))

;;(length2 (list 1 2 3 4 5))

;;ex2.34

(defun horner-eval (x coefficient-sequence)
  (m-accumulate (lambda (this-coeff higher-terms)
                  (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))

;;(horner-eval 2 (list 1 3 0 5 0 1)) -> 79

;;ex2.35
(defun count-leaves2 (tree)
  (m-accumulate (lambda (x y) (if (listp x)
                                  (+ (count-leaves2 x) y)
                                (+ 1 y)))
                0
                tree))

;;(count-leaves2 (list 1 2 (list 3 4 (list 5 6)))) -> 6

;;ex2.36

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
    (cons (m-accumulate op init (m-map 'car seqs))
          (accumulate-n op init (m-map 'cdr seqs)))))

;;(accumulate-n '+ 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) -> (22 26 30)

;;ex2.37

(defconst xm (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(defun m-map-n (proc seqs)
  (if (null (car seqs))
      nil
    (cons (apply proc (m-map 'car seqs))
          (m-map-n proc (m-map 'cdr seqs)))))

(defun dot-product (v w)
  (m-accumulate '+ 0 (m-map-n '* (list v w))))

(dot-product (list 1 2 3) (list 4 5 6))

(defun matrix-*-vector (m v)
  (m-map (lambda (mr)
           (dot-product mr v)) m))

(defconst matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
;;(matrix-*-vector matrix (list 2 3 4 5)) -> (40 96 152)

(defun transpose (mat)
  (accumulate-n 'cons nil mat))

;;(transpose xm) -> ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(defun matrix-*-matrix (m n)
  (let ((n-cols (transpose n)))
    (m-map (lambda (m-row)
             (m-map (lambda (n-col)
                      (dot-product m-row n-col))
                    n-cols))
           m)))
(defconst matrix2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) 
;;(matrix-*-matrix matrix matrix2) -> ((70 80 90) (158 184 210) (246 288 330))


;;ex2.38

(defun fold-left (op initial sequence)
  (letrec ((iter (lambda (result rest)
                   (if (null rest)
                       result
                     (funcall iter (funcall op result (car rest))
                              (cdr rest))))))
    (funcall iter initial sequence)))

(defalias 'fold-right 'm-accumulate)

;;(fold-left '/ 1.0 (list 1 2 3)) -> 0.16666666666666666
;;(fold-right '/ 1.0 (list 1 2 3)) -> 1.5

;;(fold-right 'list nil (list 1 2 3)) -> (1 (2 (3 nil)))
;;(fold-left 'list nil (list 1 2 3)) -> (((nil 1) 2) 3)

;;ex2.39

(defun reverse-right (sequence)
  (fold-right (lambda (x acc) (m-append acc (list x))) nil sequence))

(defun reverse-left (sequence)
  (fold-left (lambda (acc x) (cons x acc)) nil sequence))

;;(reverse-left (list 1 2 3 4 5))
;;(reverse-right (list 1 2 3 4 5))

(defun divides? (a b)
  (= (mod a b) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (t (find-divisor n (+ 1 test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= n (smallest-divisor n)))

(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun make-sum-pair (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun flatmap (proc seq)
  (m-accumulate 'm-append nil (m-map proc seq)))

(defun prime-sum-pairs (n)
  (m-map 'make-sum-pair
         (m-filter 'prime-sum?
                   (flatmap (lambda (i)
                              (m-map (lambda (j)
                                       (list i j))
                                     (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

;;(prime-sum-pairs 6)

(defun m-remove (item seq)
  (m-filter (lambda (x)
              (not (= x item)))
            seq))

;;(remove 2 (list 1 2 3))

(defun permutations (s)
  (if (null s)
      (list nil)
    (flatmap (lambda (x)
               (m-map (lambda (p) (cons x p))
                      (permutations (m-remove x s))))
             s)))

;;(permutations (list 1 2 3))

;;ex2.40

(defun unique-pairs (n)
  (flatmap (lambda (j)
             (m-map (lambda (i)
                      (list i j))
                    (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 n)))

;;(unique-pairs 5)

(defun prime-sum-pairs2 (n)
  (m-map 'make-sum-pair
         (m-filter 'prime-sum?
                   (unique-pairs n))))

;;(prime-sum-pairs2 6)

;;ex2.41

(defun list-sum? (s seq)
  (= s (m-accumulate '+ 0 seq)))

(defun ordered-triplets-sum (n s)
  (m-filter (apply-partially #'list-sum? s)
            (flatmap (lambda (i)
                       (flatmap (lambda (j)
                                  (m-map (lambda (k)
                                           (list k j i))
                                         (enumerate-interval 1 (- j 1))))
                                (enumerate-interval 1 i)))
                     (enumerate-interval 1 n))))

;;(ordered-triplets-sum 7 10) -> ((2 4 4) (2 3 5) (1 4 5) (1 3 6) (1 2 7))

;;ex2.42

(defconst empty-board nil)

;;(((row1 col1) (row2 col2) ...) ......)

(defun safe? (k positions) 
  (let ((new-row (caar positions)) 
        (new-col (cadar positions)) 
        (rest (cdr positions))) 
    (eval
     (cons 'and
           (m-map
            (lambda (pos) 
              (let ((row (car pos)) 
                    (col (cadr pos))) 
                (and (not (= new-col col))
                     (not (= new-row row))
                     (not (= (abs (- new-row row)) 
                             (abs (- new-col col))))))) 
            rest)))))


(defun adjoin-position (row col rest-of-queens)
  (cons (list row col) rest-of-queens))

(defun queens (board-size)
  (letrec ((queen-cols (lambda (k)
                         (if (= k 0)
                             (list empty-board)
                           (m-filter
                            (lambda (positions) (safe? k positions))
                            (flatmap
                             (lambda (rest-of-queens)
                               (m-map (lambda (new-row)
                                        (adjoin-position new-row k rest-of-queens))
                                      (enumerate-interval 1 board-size)))
                             (funcall queen-cols (- k 1))))))))
    (funcall queen-cols board-size)))

;;(let ((max-lisp-eval-depth 100000) (max-specpdl-size 100000)) (length (queens 8))) -> 92

;;ex2.43-2.52
;; TODO: Implement picture language in elisp

(defun memq (item seq)
  (cond ((null seq) nil)
        ((eq item (car seq)) seq)
        (:else (memq item (cdr seq)))))

;;(memq 'apple '(pear apple banana)) -> (apple banana)

;;ex2.53

;;(list 'a 'b 'c) -> (a b c)
;;(list (list 'george)) -> ((george))
;;(cdr '((x1 x2) (y1 y2))) -> ((y1 y2))
;;(cadr '((x1 x2) (y1 y2))) -> (y1 y2)
;;(memq 'red '((red shoes) (blue socks))) -> nil
;;(memq 'red '(red shoes blue socks)) -> (red shoes blue socks)

;;ex2.54

(defun equal? (seq1 seq2)
  (cond ((and (null seq1) (null seq2)) t)
        ((eq (car seq1) (car seq2)) (equal? (cdr seq1) (cdr seq2)))
        (:else nil)))

;;(equal? '(this is a list) '(this is a list)) -> t

;;ex2.55
;;(car ''abracadabra) -> quote
;; (car ''something) is treated by the interpreter as (car (quote (quote something)))


(defun number? (x) (numberp x))
(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2) (and (variable? v1) (variable? v2) (eq v1 v2)))
(defun make-sum (a1 a2) (list '+ a1 a2))
(defun make-product (a1 a2) (list '* a1 a2))
(defun sum? (x) (and (listp x) (eq (car x) '+)))
(defun addend (x) (cadr x))
(defun augend (x) (caddr x))
(defun product? (x) (and (listp x) (eq (car x) '*)))
(defun multiplier (x) (cadr x))
(defun multiplicand (x) (caddr x))

(defun deriv (exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))))

;;(deriv '(+ x 3) 'x) -> (+ 1 0)
;;(deriv '(* x y) 'x) -> (+ (* x 0) (* 1 y))
;; (deriv '(* (* x y) (+ x 3)) 'x) -> (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(defun =number? (exp num)
  (and (number? exp) (= exp num)))

(defun make-sum2 (a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (:else (list '+ a1 a2))))

(defun make-product2 (a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (:else (list '* a1 a2))))

;;ex2.56

(defun exponentiation? (exp)
  (eq (car exp) '**))

(defun base (exp)
  (cadr exp))

(defun exponent (exp)
  (caddr exp))

(defun make-exponentiation (base exponent)
  (cond ((= base 1) 1)
        ((= exponent 0) 1)
        ((= exponent 1) base)
        (:else (list '** base exponent))))

(defun deriv2 (exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv2 (addend exp) var)
                   (deriv2 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv2 (multiplicand exp) var))
                   (make-product (deriv2 (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product2
          (exponent exp)
          (make-product2 (make-exponentiation (base exp) (- (exponent exp) 1))
                         (deriv2 (base exp) var))))))

;;(deriv2 '(** x 2) 'x) -> (* 2 x)

;;ex2.57
(defun make-sum-list (&rest args) (if (= 2 (length args))
                                      (apply 'make-sum2 args)
                                    (make-sum2 (car args) (make-sum-list (cdr args)))))

(defun make-product-list (&rest args) (if (= 2 (length args))
                                          (apply 'make-product2 args)
                                        (make-product2 (car args) (make-product-list (cdr args)))))

(defun augend3 (x) (let ((a (cddr x)))
                     (if (= (length a) 1)
                         (car a)
                       (apply 'make-sum-list a))))

(defun multiplicand3 (x) (let ((a (cddr x)))
                           (if (= (length a) 1)
                               (car a)
                             (apply 'make-product-list a))))

(defun deriv3 (exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum-list (deriv3 (addend exp) var)
                        (deriv3 (augend3 exp) var)))
        ((product? exp)
         (make-sum-list (make-product-list (multiplier exp)
                                           (deriv3 (multiplicand3 exp) var))
                        (make-product-list (deriv3 (multiplier exp) var)
                                           (multiplicand3 exp))))
        ((exponentiation? exp)
         (make-product-list
          (exponent exp)
          (make-product-list (make-exponentiation (base exp) (- (exponent exp) 1))
                             (deriv2 (base exp) var))))))

;;(deriv3 '(* x y (* x 3)) 'x) -> (+ (* x (* y 3)) (* y (* x 3)))
;;(deriv3 '(* x y (+ x 3)) 'x)

;; Sets as unordered lists

;;without duplicates

(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((eq x (car set)) t)
        (:else (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
    (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (:else (intersection-set (cdr set1) set2))))

;;(intersection-set (list 1 2 3 4) (list 2 4 6 8))

;;ex2.59

(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (:else (cons (car set1) (union-set (cdr set1) set2)))))

;;(union-set (list 1 2 3 4) (list 2 4 6 8))

;;ex2.60
;;allow duplicates
;; append-set is O(1), union-set is O(n) are much faster

;; Sets as ordered lists

(defun element-of-set2? (x set)
  (cond ((null set) nil)
        ((< x (car set)) nil)
        ((= x (car set)) t)
        (:else (element-of-set2? x (cdr set)))))

(defun intersection-set2 (set1 set2)
  (if (or (null set1) (null set2))
      nil
    (let ((x (car set1))
          (y (car set2)))
      (cond ((= x y) (cons x (intersection-set2 (cdr set1) (cdr set2))))
            ((< x y) (intersection-set2 (cdr set1) set2))
            (:else (intersection-set2 set1 (cdr set2)))))))

;;(intersection-set2 (list 1 2 3 4) (list 2 4 6 8))

;;ex2.61

(defun adjoin-set2 (x set)
  (if (null set)
      (list x)
    (let ((a (car set)))
      (cond ((< x a) (cons x set))
            ((= x a) set)
            (:else (cons a (adjoin-set2 x (cdr set))))))))

;;(adjoin-set2 3 (list 1 2 4))

;;ex2.62

(defun union-set2 (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        (:else (let ((a (car set1))
                     (b (car set2)))
                 (cond ((= a b) (cons a (union-set2 (cdr set1) (cdr set2))))
                       ((< a b) (cons a (union-set2 (cdr set1) set2)))
                       (:else (cons b (union-set2 set1 (cdr set2)))))))))

;;(union-set2 (list 1 2 3 4) (list 2 4 6 8))

;; Sets as binary trees

(defun entry (tree) (car tree))
(defun left-branch (tree) (cadr tree))
(defun right-branch (tree) (caddr tree))
(defun make-tree (entry left-branch right-branch) (list entry left-branch right-branch))

(defun element-of-set3? (x set)
  (cond ((null set) nil)
        ((= x (entry set)) t)
        ((< x (entry set)) (element-of-set3? x (left-branch set)))
        (:else (element-of-set3? x (right-branch set)))))

;;(element-of-set3? 3 (list 2 nil (list 4 (list 3 nil nil) nil))) -> t

(defun adjoin-set3 (x set)
  (cond ((null set) (list x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set3 x (left-branch set)) (right-branch set)))
        (:else (make-tree (entry set) (left-branch set) (adjoin-set3 x (right-branch set))))))

;;(adjoin-set3 4 (list 2 nil (list 5 (list 3 nil nil) nil))) -> (2 nil (5 (3 nil (4 nil nil)) nil))

;;ex2.63

(defun tree->list-1 (tree)
  (if (null tree)
      nil
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (letrec ((copy-to-list (lambda (tree result-list)
                           (if (null tree)
                               result-list
                             (funcall copy-to-list (left-branch tree)
                                      (cons (entry tree)
                                            (funcall copy-to-list (right-branch tree) result-list)))))))
    (funcall copy-to-list tree nil)))

(defconst tree1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;;(tree->list-1 tree1) -> (1 3 5 7 9 11)
;;(tree->list-2 tree1) -> (1 3 5 7 9 11)

;;ex2.64

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons nil elts)
    (let* ((left-size (/ (- n 1) 2))
           (left-result (partial-tree elts left-size))
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (right-size (- n (+ left-size 1)))
           (this-entry (car non-left-elts))
           (right-result (partial-tree (cdr non-left-elts)
                                       right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result)))
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

;;(list->tree (list 1 2 3 5 7 9))

;;ex2.65

(defun union-set3 (set1 set2)
  (list->tree (union-set2 (tree->list-1 set1)
                          (tree->list-1 set2))))

(defun intersection-set3 (set1 set2)
  (list->tree (intersection-set2 (tree->list-1 set1)
                                 (tree->list-1 set2))))

;;(union-set3 (list->tree (list 1 2 3 4)) (list->tree (list 2 4 6 8)))

;;ex2.66

(defun lookup (x set)
  (cond ((null set) nil)
        ((= x (entry set)) (entry set))
        ((< x (entry set)) (lookup x (left-branch set)))
        (:else (lookup x (right-branch set)))))

;;(lookup 3 (list 2 nil (list 4 (list 3 nil nil) nil))) -> 3

;;Huffman Encoding Trees

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (object)
  (eq 'leaf (car object)))

(defun symbol-leaf (x) (cadr x))

(defun weight-leaf (x) (caddr x))

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun left-branch (tree) (car tree))
(defun right-branch (tree) (cadr tree))

(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
    (caddr tree)))

(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
    (cadddr tree)))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (:else (error "bad bit -- %d -- choose-branch" bit))))

(defun decode (bits tree)
  (letrec ((decode-1 (lambda (bits current-branch)
                       (if (null bits)
                           '()
                         (let ((next-branch (choose-branch (car bits) current-branch)))
                           (if (leaf? next-branch)
                               (cons (symbol-leaf next-branch)
                                     (funcall decode-1 (cdr bits) tree))
                             (funcall decode-1 (cdr bits) next-branch)))))))
    (funcall decode-1 bits tree)))

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (:else (cons (car set)
                     (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

;;ex2.67

(defconst sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(defconst sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;(decode sample-message sample-tree)-> (A D A B B C A)

;;ex2.68

(defun encode-symbol (symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (:else (error "symbol not in tree - %s" symbol))))

(defun encode (message tree)
  (if (null message)
      '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

;;(encode (decode sample-message sample-tree) sample-tree) -> (0 1 1 0 0 1 0 1 0 1 1 1 0)

;;ex2.69

(defun succesive-merge (leaf-set)
  (if (null (cdr leaf-set))
      (car leaf-set)
    (succesive-merge (adjoin-set (make-code-tree (cadr leaf-set)
                                                 (car leaf-set))
                                 (cddr leaf-set)))))

(defun generate-huffman-tree (pairs)
  (succesive-merge (make-leaf-set pairs)))

;;(generate-huffman-tree (list (list 1 6) (list 3 7) (list 2 8) (list 4 9)))

;;ex2.70

(defconst hf-list (list (list 'na  16) (list 'boom 1) (list 'a 2) (list 'get 2) (list 'job 2) (list 'sha  3) (list 'yip  9) (list 'wah 1)))

(defconst mes1 (list 'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na 'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na 'wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'sha 'boom))

;;(length mes1) -> 36
;;(length (encode mes1 (generate-huffman-tree hf-list))) -> 84

;;ex2.71 -> 1,n-1

;;2.4 - Multiple Representations for Abstract Data

(defun attach-tag (type-tag contents)
  (if (eq type-tag 'elisp-number)
      contents
    (cons type-tag contents)))

(defun type-tag (datum)
  (cond ((listp datum) (car datum))
        ((numberp datum) 'elisp-number)
        (:else (error "Bad tagged datum -- TYPE-TAG"))))

(defun contents (datum)
  (cond ((listp datum) (cdr datum))
        ((numberp datum) datum)
        (:else (error "Bad tagged datum -- CONTENTS"))))

;;
(setq real-part (make-hash-table :test 'equal))
(setq imag-part (make-hash-table :test 'equal))
(setq magnitude (make-hash-table :test 'equal))
(setq angle (make-hash-table :test 'equal))
(setq make-from-real-imag-map (make-hash-table :test 'equal))
(setq make-from-mag-ang-map (make-hash-table :test 'equal))

(defun install-rectangular-package ()
  (letrec ((real-part-fn (lambda (z) (car z)))
           (imag-part-fn (lambda (z) (cdr z)))
           (make-from-real-imag-fn (lambda (x y) (cons x y)))
           (magnitude-fn (lambda (z)
                           (sqrt (+ (square (funcall real-part-fn z))
                                    (square (funcall imag-part-fn z))))))
           (angle-fn (lambda (z) (atan (funcall imag-part-fn z) (funcall real-part-fn z))))
           (make-from-mag-ang-fn (lambda (r a)
                                   (cons (* r (cos a)) (* r (sin a)))))
           (tag-fn (lambda (x) (attach-tag 'rectangular x))))
    (puthash '(rectangular) real-part-fn real-part)
    (puthash '(rectangular) imag-part-fn imag-part)
    (puthash '(rectangular) magnitude-fn magnitude)
    (puthash '(rectangular) angle-fn angle)
    (puthash 'rectangular (lambda (x y) (funcall tag-fn (funcall make-from-real-imag-fn x y))) make-from-real-imag-map)
    (puthash 'rectangular (lambda (r a) (funcall tag-fn (funcall make-from-mag-ang-fn r a))) make-from-mag-ang-map)))

(install-rectangular-package)

;;(funcall (gethash '(rectangular) real-part) (cons 1 2))
;;(funcall (gethash 'rectangular make-from-real-imag-map) 1 2)
;;(funcall (gethash '(rectangular) magnitude) (cons 1 2))

(defun install-polar-package ()
  (letrec ((real-part-fn (lambda (z) (* (car z) (cos (cdr z)))))
           (imag-part-fn (lambda (z) (* (car z) (sin (cdr z)))))
           (make-from-real-imag-fn (lambda (x y) (cons (sqrt (+ (square x) (square y))) (atan (/ y x)))))
           (magnitude-fn (lambda (z) (car z)))
           (angle-fn (lambda (z) (cdr z)))
           (make-from-mag-ang-fn (lambda (r a) (cons r a)))
           (tag-fn (lambda (x) (attach-tag 'rectangular x))))
    (puthash '(polar) real-part-fn real-part)
    (puthash '(polar) imag-part-fn imag-part)
    (puthash '(polar) magnitude-fn magnitude)
    (puthash '(polar) angle-fn angle)
    (puthash 'polar (lambda (x y) (funcall tag-fn (funcall make-from-real-imag-fn x y))) make-from-real-imag-map)
    (puthash 'polar (lambda (r a) (funcall tag-fn (funcall make-from-mag-ang-fn r a))) make-from-mag-ang-map)))

(install-polar-package)

;;(funcall (gethash '(polar) real-part) (cons 5 1))
;;(funcall (gethash 'polar make-from-real-imag-map) 3 4)
;;(funcall (gethash '(polar) magnitude) (cons 1 2))

(defun apply-generic (op &rest args)
  (letrec ((type-tags (m-map 'type-tag args))
           (proc (gethash type-tags op)))
    (if proc
        (apply proc (m-map 'contents args))
      (error "no method for these types -- APPLY-GENERIC"))))

;;(apply-generic magnitude (cons 'rectangular (list 3 4))) -> 5.0


;;ex2.73

(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

(defun deriv5 (exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (:else (funcall (gethash (operator exp) deriv-map) (operands exp) var))))

(setq deriv-map (make-hash-table :test 'equal))

(defun install-deriv-package ()
  (let* ((sum-fn (lambda (exp var)
                   (make-sum (deriv5 (car exp) var)
                             (deriv5 (cadr exp) var))))
         (product-fn (lambda (exp var)
                       (make-sum (make-product (cadr exp) (deriv5 (car exp) var))
                                 (make-product (car exp) (deriv5 (cadr exp) var))))))
    (puthash '+ sum-fn deriv-map)
    (puthash '* product-fn deriv-map)))
(install-deriv-package)

;;(deriv5 '(+ x 1) 'x) -> (+ 1 0)
;;(deriv5 '(* x y) 'x) -> (+ (* y 1) (* x 0)) 

;;ex2.75

(defun make-from-mag-ang (r a)
  (letrec ((dispatch (lambda (op)
                       (cond ((eq op 'real-part) (* r (cos a)))
                             ((eq op 'img-part) (* r (sin a)))
                             ((eq op 'magnitude) r)
                             ((eq op 'angle) a)
                             (:else (error "Unknown op -- MAKE-FROM-MAG-ANG"))))))
    dispatch))

(defun apply-generic2 (op arg) (funcall arg op))

;;(apply-generic2 'real-part (make-from-mag-ang 5 1))

;;ex2.76
;; new types - message-passing , new operations- data-directed

;;2.5.1
(defun n-apply-generic (op &rest args)
  (letrec ((type-tags (m-map 'type-tag args))
           (proc (gethash type-tags op)))
    (if proc
        (apply proc (m-map 'contents args))
      (error "no method for these types -- N-APPLY-GENERIC"))))

(defun add (x y) (n-apply-generic add-map x y))
(defun sub (x y) (n-apply-generic sub-map x y))
(defun mul (x y) (n-apply-generic mul-map x y))
(defun div (x y) (n-apply-generic div-map x y))

(defun equ? (x y) (n-apply-generic equ?-map x y))
(defun =zero? (x) (n-apply-generic =zero?-map x))

(setq add-map (make-hash-table :test 'equal))
(setq sub-map (make-hash-table :test 'equal))
(setq mul-map (make-hash-table :test 'equal))
(setq div-map (make-hash-table :test 'equal))
(setq make-map (make-hash-table :test 'equal))

(setq equ?-map (make-hash-table :test 'equal))
(setq =zero?-map (make-hash-table :test 'equal))

(defun install-elisp-number-package ()
  (let* ((tag (lambda (x)
                (attach-tag 'elisp-number x))))
    (puthash (list 'elisp-number 'elisp-number) (lambda (x y) (funcall tag (+ x y))) add-map)
    (puthash '(elisp-number elisp-number) (lambda (x y) (funcall tag (- x y))) sub-map)
    (puthash '(elisp-number elisp-number) (lambda (x y) (funcall tag (* x y))) mul-map)
    (puthash '(elisp-number elisp-number) (lambda (x y) (funcall tag (/ x y))) div-map)
    (puthash 'elisp-number (lambda (x) (funcall tag x)) make-map)
    (puthash '(elisp-number elisp-number) (lambda (x y) (= x y)) equ?-map)
    (puthash '(elisp-number) (lambda (x) (= x 0)) =zero?-map)))

(install-elisp-number-package)

;;(add (cons 'elisp-number 3) (cons 'elisp-number 4)) -> (elisp-number . 7)

(defun elisp-number (x)
  (funcall (gethash 'elisp-number make-map) x))

;;(elisp-number 10) -> (elisp-number . 10)

(defun contents (exp)
  (if (listp exp)
      (cdr exp)
    (error "Bad tagged datum -- CONTENTS")))

(defun install-rational-package ()
  (let* ((numer (lambda (x) (car x)))
         (denom (lambda (x) (cdr x)))
         (make-rat-int (lambda (n d)
                         (let* ((g (gcd n d)))
                           (cons (/ n g) (/ d g)))))
         (add-rat (lambda (x y)
                    (funcall make-rat-int
                             (+ (* (funcall numer x) (funcall denom y))
                                (* (funcall numer y) (funcall denom x)))
                             (* (funcall denom x) (funcall denom y)))))
         (sub-rat (lambda (x y)
                    (funcall make-rat-int
                             (- (* (funcall numer x) (funcall denom y))
                                (* (funcall numer y) (funcall denom x)))
                             (* (funcall denom x) (funcall denom y)))))
         (mul-rat (lambda (x y)
                    (funcall make-rat-int
                             (* (funcall numer x) (funcall numer y))
                             (* (funcall denom x) (funcall denom y)))))
         (div-rat (lambda (x y)
                    (funcall make-rat-int
                             (* (funcall numer x) (funcall denom y))
                             (* (funcall denom x) (funcall numer y)))))
         (tag (lambda (x) (attach-tag 'rational x))))
    (puthash '(rational rational) (lambda (x y) (funcall tag (funcall add-rat x y))) add-map)
    (puthash '(rational rational) (lambda (x y) (funcall tag (funcall sub-rat x y))) sub-map)
    (puthash '(rational rational) (lambda (x y) (funcall tag (funcall mul-rat x y))) mul-map)
    (puthash '(rational rational) (lambda (x y) (funcall tag (funcall div-rat x y))) div-map)
    (puthash 'rational (lambda (n d) (funcall tag (make-rat n d))) make-map)
    (puthash '(rational rational) (lambda (x y) (and (equ? (funcall numer x) (funcall numer y))
                                                     (equ? (funcall denom x) (funcall denom y))))
             equ?-map)
    (puthash '(rational) (lambda (x) (= 0 (numer x))) =zero?-map)))

(install-rational-package)

(defun rational-number (n d)
  (funcall (gethash 'rational make-map) n d))

;;(rational-number 1 2) -> (rational 1 . 2)
;;(add (rational-number 1 2) (rational-number 3 4)) -> (rational 5 . 4)

(defun real-part-fn (z)
  (apply-generic real-part z))

(defun imag-part-fn (z)
  (apply-generic imag-part z))

(defun magnitude-fn (z)
  (apply-generic magnitude z))

(defun angle-fn (z)
  (apply-generic angle z))

(defun make-from-real-imag (x y)
  (apply-generic make-from-real-imag-map x y))

(defun make-from-mag-ang (x y)
  (apply-generic make-from-real-mag-ang x y))

(real-part-fn (list 'rectangular 1 2))

(defun install-complex-package ()
  (letrec ((make-from-real-imag (lambda (x y)
                                  (funcall (gethash 'rectangular make-from-real-imag-map) x y)))
           (make-from-mag-ang (lambda (r a)
                                (funcall (gethash 'polar make-from-mag-ang-map) r a)))
           (add-complex (lambda (z1 z2)
                          (funcall make-from-real-imag
                                   (+ (real-part-fn z1) (real-part-fn z2))
                                   (+ (imag-part-fn z1) (imag-part-fn z2)))))
           (sub-complex (lambda (z1 z2)
                          (funcall make-from-real-imag
                                   (- (real-part-fn z1) (real-part-fn z2))
                                   (- (imag-part-fn z1) (imag-part-fn z2)))))
           (mul-complex (lambda (z1 z2)
                          (funcall make-from-mag-ang
                                   (* (magnitude-fn z1) (magnitude-fn z2))
                                   (+ (angle-fn z1) (angle-fn z2)))))
           (div-complex (lambda (z1 z2)
                          (funcall make-from-mag-ang
                                   (/ (magnitude-fn z1) (magnitude-fn z2))
                                   (- (angle-fn z1) (angle-fn z2)))))
           (tag (lambda (x) (attach-tag 'complex x))))
    (puthash '(complex complex) (lambda (x y) (funcall tag (funcall add-complex x y))) add-map)
    (puthash '(complex complex) (lambda (x y) (funcall tag (funcall sub-complex x y))) sub-map)
    (puthash '(complex complex) (lambda (x y) (funcall tag (funcall mul-complex x y))) mul-map)
    (puthash '(complex complex) (lambda (x y) (funcall tag (funcall mul-complex x y))) mul-map)
    (puthash 'complex (lambda (x y) (funcall tag (funcall make-from-real-imag x y))) make-from-real-imag-map)
    (puthash 'complex (lambda (x y) (funcall tag (funcall make-from-mag-ang x y))) make-from-mag-ang-map)
    (puthash '(complex complex) (lambda (x y) (and (equ? (magnitude-fn x) (magnitude-fn y))
                                                   (equ? (angle-fn x) (angle-fn y))))
             equ?-map)
    (puthash '(complex) (lambda (z) (and (= 0 (real-part-fn z))
                                         (= 0 (imag-part-fn z))))
             =zero?-map)))

(install-complex-package)

(defun make-complex-from-real-imag (x y)
  (funcall (gethash 'complex make-from-real-imag-map) x y))

(defun make-complex-from-mag-ang (x y)
  (funcall (gethash 'complex make-from-mag-ang-map) x y))

;;(add (make-complex-from-real-imag 1 2) (make-complex-from-mag-ang 5 0)) -> (complex rectangular 6 . 2)

;; solutions to ex2.79 + ex2.80 are included above
;;(equ? 2 2) -> t
;;(equ? (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))-> t

;;(=zero? 0) -> t
;;(=zero? (make-complex-from-real-imag 0 0)) -> t
;;(=zero? (rational-number 0 3)) -> t

;;2.5.3

(defun contents (datum)
  (cond ((listp datum) (cdr datum))
        ((numberp datum) datum)
        (:else (error "Bad tagged datum -- CONTENTS"))))

(defun install-polynomial-package ()
  (letrec ((make-poly (lambda (var term-list)
                        (cons var term-list)))
           (variable (lambda (p) (car p)))
           (term-list-fn (lambda (p) (cdr p)))
           (variable? (lambda (x) (symbolp x)))
           (same-variable? (lambda (v1 v2) (and (funcall variable? v1) (funcall variable? v2) (eq v1 v2))))
           (the-empty-termlist (lambda () '()))
           (first-term (lambda (term-list) (car term-list)))
           (rest-terms (lambda (term-list) (cdr term-list)))
           (empty-termlist? (lambda (term-list) (null term-list)))
           (make-term (lambda (order coeff) (list order coeff)))
           (order (lambda (term) (car term)))
           (coeff (lambda (term) (cadr term)))
           (adjoin-term (lambda (term term-list)
                          (if (=zero? (funcall coeff term))
                              term-list
                            (cons term term-list))))
           (=zero? (lambda (p)
                     (m-accumulate (lambda (x y) (and x y)) t
                                   (m-map (lambda (x) (= 0 (funcall coeff x)))
                                          (funcall term-list-fn p)))))
           (add-terms (lambda (l1 l2)
                        (cond ((funcall empty-termlist? l1) l2)
                              ((funcall empty-termlist? l2) l1)
                              (:else (let* ((t1 (funcall first-term l1))
                                            (t2 (funcall first-term l2)))
                                       (cond ((> (funcall order t1) (funcall order t2))
                                              (funcall adjoin-term t1
                                                       (funcall add-terms (funcall rest-terms l1) l2)))
                                             ((< (funcall order t1) (funcall order t2))
                                              (funcall adjoin-term t2
                                                       (funcall add-terms l1 (funcall rest-terms l2))))
                                             (:else (funcall adjoin-term
                                                             (funcall make-term (funcall order t1)
                                                                      (add (funcall coeff t1) (funcall coeff t2)))
                                                             (funcall add-terms (funcall rest-terms l1)
                                                                      (funcall rest-terms l2))))))))))
           (add-poly (lambda (p1 p2) (if (funcall same-variable? (funcall variable p1) (funcall variable p2))
                                         (funcall make-poly
                                                  (funcall variable p1)
                                                  (funcall add-terms
                                                           (funcall term-list-fn p1)
                                                           (funcall term-list-fn p2)))
                                       (error "Polys not in same var -- ADD-POLY"))))
           (mul-term-by-all-terms (lambda (term L)
                                    (if (funcall empty-termlist? L)
                                        (funcall the-empty-termlist)
                                      (let ((t1 (funcall first-term L)))
                                        (funcall adjoin-term
                                                 (funcall make-term
                                                          (+ (funcall order term) (funcall order t1))
                                                          (* (funcall coeff term) (funcall coeff t1)))
                                                 (funcall mul-term-by-all-terms term
                                                          (funcall rest-terms L)))))))
           (mul-terms (lambda (l1 l2)
                        (if (funcall empty-termlist? l1)
                            (funcall the-empty-termlist)
                          (funcall add-terms
                                   (funcall mul-term-by-all-terms (funcall first-term l1) l2)
                                   (funcall mul-terms (funcall rest-terms l1) l2)))))
           (mul-poly (lambda (p1 p2) (if (funcall same-variable? (funcall variable p1) (funcall variable p2))
                                         (funcall make-poly
                                                  (funcall variable p1)
                                                  (funcall mul-terms (funcall term-list-fn p1)
                                                           (funcall term-list-fn p2)))
                                       (error "Polys not in same var -- MUL-POLY"))))
           (tag (lambda (p) (attach-tag 'polynomial p))))
    (puthash '(polynomial polynomial) (lambda (p1 p2) (funcall tag (funcall add-poly p1 p2))) add-map)
    (puthash '(polynomial polynomial) (lambda (p1 p2) (funcall tag (funcall mul-poly p1 p2))) mul-map)
    (puthash 'polynomial  (lambda (var terms) (funcall tag (funcall make-poly var terms))) make-map)
    (puthash '(polynomial) (lambda (p) (funcall =zero? p)) =zero?-map)))

(install-polynomial-package)

(defun make-poly (x y)
  (funcall (gethash 'polynomial make-map) x y))

;;(add (make-poly 'x (list (list 2 4) (list 1 2))) (make-poly 'x (list (list 1 2)))) -> (polynomial x (2 4) (1 4))
;;(mul (make-poly 'x (list (list 2 4) (list 1 2))) (make-poly 'x (list (list 1 2)))) -> (polynomial x (3 8) (2 4))

;;ex2.87
;;(=zero? (make-poly 'x (list (list 1 0)))) -> t
