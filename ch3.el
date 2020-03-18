;;; -*- lexical-binding: t; -*-

(defun make-account (balance)
  (let* ((withdraw (lambda (amount)
                     (if (>= balance amount)
                         (progn
                           (setq balance (- balance amount))
                           balance)
                       (error "Insufficient funds -- WITHDRAW"))))
         (deposit (lambda (amount)
                    (progn
                      (setq balance (+ balance amount))
                      balance)))
         (dispatch (lambda (method)
                     (cond ((eq method 'withdraw) withdraw)
                           ((eq method 'deposit) deposit)
                           (:else (error "Unknown request -- MAKE-ACCOUNT"))))))
    dispatch))

(fset 'acc (make-account 100))
;;(funcall (acc 'withdraw) 20) -> 80
;;(funcall (acc 'deposit) 30) -> 110

;;ex3.1

(defun make-accumulator (acc)
  (let* ((accumulate (lambda (x)
                       (progn
                         (setq acc (+ acc x))
                         acc))))
    accumulate))

(fset 'A (make-accumulator 5))

;;(A 10) -> 15
;;(A 10) -> 25

;;ex3.2

(defun make-monitered (f)
  (let* ((counter 0)
         (mf (lambda (x)
               (cond ((eq x 'how-many-calls?) counter)
                     ((eq x 'reset-counter) (setq counter 0))
                     (:else (progn
                              (setq counter (+ counter 1))
                              (funcall f x)))))))
    mf))

(fset 's (make-monitered 'sqrt))

;;(s 100) -> 10.0
;;(s 'how-many-calls?) -> 1

;;ex3.3

(defun make-protected-account (secret-password balance)
  (let* ((withdraw (lambda (amount)
                     (if (>= balance amount)
                         (progn (setq balance (- balance amount))
                                balance)
                       (error "Insufficient funds -- WITHDRAW"))))
         (deposit (lambda (amount)
                    (progn (setq balance (+ balance amount))
                           balance)))
         (dispatch (lambda (pass method)
                     (cond ((not (eq pass secret-password)) (error "Incorrect Password"))
                           ((eq method 'withdraw) withdraw)
                           ((eq method 'deposit) deposit)
                           (:else (error "Unknown request -- MAKE-ACCOUNT"))))))
    dispatch))

(fset 'p-acc (make-protected-account 'qwerty 100))

;;(funcall (p-acc'qwerty 'withdraw) 20) -> 80
;;(funcall (p-acc 'qwert 'withdraw) 30) -> Debugger entered--Lisp error: (error "Incorrect Password")
;;(funcall (p-acc 'qwerty 'withdraw) 30) -> 50

;;ex3.4

(defun make-protected-account-with-cops (secret-password balance)
  (let* ((wrong-pass-counter 0)
         (call-the-cops (lambda ()
                          (message "Calling the cops")))
         (withdraw (lambda (amount)
                     (if (>= balance amount)
                         (progn
                           (setq balance (- balance amount))
                           balance)
                       (error "Insufficient funds -- WITHDRAW"))))
         (deposit (lambda (amount)
                    (progn
                      (setq balance (+ balance amount))
                      balance)))
         (dispatch (lambda (pass method)
                     (cond ((not (eq pass secret-password))
                            (if (>= wrong-pass-counter 2)
                                (lambda (_) (funcall call-the-cops))
                              (progn
                                (setq wrong-pass-counter (+ wrong-pass-counter 1))
                                (error "Incorrect Password"))))
                           ((eq method 'withdraw) withdraw)
                           ((eq method 'deposit) deposit)
                           (:else (error "Unknown request -- MAKE-ACCOUNT"))))))
    dispatch))

(fset 'pc-acc (make-protected-account-with-cops 'qwerty 100))

;;(funcall (pc-acc'qwerty 'withdraw) 20)
;;(funcall (pc-acc 'qwert 'withdraw) 30)


(defconst random-init 0)

(defun rand-update (x)
  (let ((a 27)
        (b 26)
        (m 127))
    (mod (+ (* a x)
            b)
         m)))

(defun rand ()
  (let ((x random-init))
    (lambda ()
      (setq x (rand-update x))
      x)))

(fset 'm-random (rand))

;; gives a "rand" int between 0 and 126
;;(m-random)

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (mod a b))))

(defun cesaro-test ()
  (= (gcd (m-random) (m-random)) 1))

(defun monte-carlo (trials experiment)
  (letrec ((iter (lambda (trials-remaining trials-passed)
                   (cond ((= trials-remaining 0) (/ trials-passed trials))
                         ((funcall experiment) (funcall iter
                                                        (- trials-remaining 1)
                                                        (+ trials-passed 1)))
                         (:else (funcall iter
                                         (- trials-remaining 1)
                                         trials-passed))))))
    (funcall iter trials 0)))

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials 'cesaro-test))))

;;(let ((max-lisp-eval-depth 10000) (max-specpdl-size 10000)) (estimate-pi 1000.0)) -> 3.383841260962061

;;ex3.5
(defun square (x) (* x x))

(defun random-in-range (low high)
  (+ low (random (- high low))))

(defun lies-in-circle? (x y)
  (<= (+ (square (- x 5))
         (square (- y 7)))
      9))

(defun estimate-integral (predicate? x1 x2 y1 y2 trials)
  (let* ((experiment (lambda ()
                       (funcall predicate? (random-in-range x1 x2) (random-in-range y1 y2))))
         (prob (monte-carlo trials experiment)))
    (/ (* prob (abs (- x1 x2)) (abs (- y1 y2))) 9)
    ))

;;(let ((max-lisp-eval-depth 10000) (max-specpdl-size 10000)) (estimate-integral 'lies-in-circle? 2 8 4 10 1000.0)) -> 3.1040000000000005

;;ex3.6

(defun rand-update2 (x) (+ x 1))
(defun rand2 ()
  (let* ((x 0)
         (dispatch (lambda (proc)
                     (if (eq proc 'reset)
                         (lambda (val) (setq x val))
                       (lambda ()
                         (setq x (rand-update2 x))
                         x)))))
    dispatch))

(fset 'r (rand2))
;;(funcall (r 'generate)) -> 1
;;(funcall (r 'reset) 10) -> 10
;;(funcall (r 'generate)) -> 11

;;ex3.7
(defun make-joint (prot-acc original-pass new-pass)
  (lambda (pass proc)
    (if (eq pass new-pass)
        (funcall prot-acc original-pass proc)
      (funcall prot-acc nil proc))))


(fset 'pc-acc2 (make-joint 'pc-acc 'qwerty 'qwerty2))

;;(funcall (pc-acc2 'qwerty2 'withdraw) 10)
;;(funcall (pc-acc2 'qwerty3 'withdraw) 10)

;;ex3.8

(fset 'some-f (let ((x 1))
                (lambda (n)
                  (setq x (* x n))
                  x)))

;;3.12
;;'(b) '(b c d)

;;3.13

(defun last-pair (x)
  (if (null (cdr x))
      x
    (last-pair (cdr x))))

(defun make-cycle (x)
  (setcdr (last-pair x) x)
  x)

;;(last-pair (make-cycle (list 1 2))) -> error: "Lisp nesting exceeds ‘max-lisp-eval-depth’"

;;3.14

(defun mystery (x)
  (letrec ((loop (lambda (x y)
                   (if (null x)
                       y
                     (let ((temp (cdr x)))
                       (setcdr x y)
                       (funcall loop temp x))))))
    (funcall loop x '())))

(defconst v (list 'a 'b 'c 'd))
;;(mystery v) -> (d c b a) ;; v -> (a)

;;3.17

(defun count-pairs (x)
  (letrec ((encountered '())
           (iter (lambda (x)
                   (if (or (not (listp x))
                           (memq x encountered))
                       0
                     (progn (setq encountered (cons x encountered))
                            (+ (funcall iter (car x))
                               (funcall iter (cdr x))
                               1))))))
    (funcall iter x)))


(defconst x (cons (cons 1 2) 3))
(setcdr x x)
;;(count-pairs x) -> 2

;;3.18

(defun has-cycle? (x)
  (letrec ((encountered '())
           (iter (lambda (x)
                   (cond ((null x) nil)
                         ((memq x encountered) t)
                         (:else (progn
                                  (setq encountered (cons x encountered))
                                  (funcall iter (cdr x))))))))
    (funcall iter x)))

;;(has-cycle? x) -> t

;;3.19
;;https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare

(defun has-cycle2? (x)
  (letrec ((iter (lambda (a b)
                   (cond ((eq a b) t)
                         ((or (null (cdr b)) (null (cddr b))) nil)
                         (:else (funcall iter (cdr a) (cddr b)))))))
    (funcall iter x (cdr-safe x))))

;;(has-cycle2? x)

;;3.3.2 Queues

(defun make-queue () (cons '() '()))

(defun front-ptr (queue) (car queue))

(defun rear-ptr (queue) (cdr queue))

(defun empty-queue? (queue) (null (front-ptr queue)))

(defun set-front-ptr! (queue item) (setcar queue item))

(defun set-rear-ptr! (queue item) (setcdr queue item))

(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
    (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (:else
           (setcdr (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(defun delete-queue! (queue)
  (if (empty-queue? queue)
      (error "DELETE called with an empty queue")
    (progn (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))

;;ex3.21

(defun print-queue (queue)
  (message "%S" (front-ptr queue)))


(defconst q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(print-queue (delete-queue! q1))

;;3.22

(defun make-queue-proc ()
  (letrec ((front-ptr '())
           (rear-ptr '())
           (queue (lambda () (cons front-ptr rear-ptr)))
           (empty-queue? (lambda () (null front-ptr)))
           (set-front-ptr! (lambda (x) (setq front-ptr x)))
           (set-rear-ptr! (lambda (x) (setq rear-ptr x)))
           (insert-queue2! (lambda (x)
                             (let ((new-val (cons x '())))
                               (cond ((funcall empty-queue?)
                                      (setq front-ptr new-val)
                                      (setq rear-ptr new-val)
                                      (funcall queue))
                                     (:else
                                      (setcdr rear-ptr new-val)
                                      (setq rear-ptr new-val)
                                      (funcall queue))))))
           (delete-queue! (lambda ()
                            (if (funcall empty-queue?)
                                (error "DELETE called with empty-queue")
                              (progn (setq front-ptr (cdr front-ptr))
                                     (funcall queue)))))
           (dispatch (lambda (proc)
                       (cond ((eq proc 'make-queue) (funcall queue))
                             ((eq proc 'queue) (funcall queue))
                             ((eq proc 'front-ptr) front-ptr)
                             ((eq proc 'rear-ptr) rear-ptr)
                             ((eq proc 'empty-queue?) (funcall empty-queue?))
                             ((eq proc 'set-front-ptr!) set-front-ptr!)
                             ((eq proc 'set-rear-ptr!) set-rear-ptr!)
                             ((eq proc 'insert-queue!) insert-queue2!)
                             ((eq proc 'delete-queue!) delete-queue!)))))
    dispatch))

;;(fset 'make-q2 (make-queue-proc))
;;(funcall (make-q2 'insert-queue!) 'a) -> ((a) a)
;;(funcall (make-q2 'insert-queue!) 'b) -> ((a b) b)
;;(funcall (make-q2 'delete-queue!)) -> ((b) b)

;;ex3.23
;;using a double-linked-list

(defun make-deque () (cons '() '()))
(defun front-ptr (deque) (car deque))
(defun rear-ptr (deque) (cdr deque))
(defun empty-deque? (deque) (null (car deque)))
(defun set-front-ptr-deque! (deque item) (setcar deque item))
(defun set-rear-ptr-deque! (deque item) (setcdr deque item))
(defun front-deque (deque) (caaar deque))

(defun front-insert-deque! (deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr-deque! deque new-pair)
           (set-rear-ptr-deque! deque new-pair)
           deque)
          (:else
           (setcdr new-pair (front-ptr deque))
           (setcdr (car (front-ptr deque)) new-pair)
           (set-front-ptr-deque! deque new-pair)
           deque))))

(defun rear-insert-deque! (deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr-deque! deque new-pair)
           (set-rear-ptr-deque! deque new-pair)
           deque)
          (:else
           (setcdr (car new-pair) (rear-ptr deque))
           (setcdr (rear-ptr deque) new-pair)
           (set-rear-ptr-deque! deque new-pair)
           deque))))

(defun front-delete-deque! (deque)
  (cond ((empty-deque? deque)
         (error "Delete called with empty deque"))
        ((eq (front-ptr deque) (rear-ptr deque))
         (setcar deque nil)
         (setcdr deque nil)
         deque)
        (:else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         (setcdr (car (front-ptr deque)) nil)
         deque)))

(defun rear-delete-deque! (deque)
  (cond ((empty-deque? deque)
         (error "DELETE called with empty deque"))
        ((eq (front-ptr deque) (rear-ptr deque))
         (setcar deque nil)
         (setcdr deque nil)
         deque)
        (:else
         (set-rear-ptr-deque! deque (cdar (rear-ptr deque)))
         (setcdr (rear-ptr deque) nil)
         deque)))

;;(setq dq (make-deque))
;;(front-insert-deque! dq 1)
;;(rear-insert-deque! dq 2)
;;(front-delete-deque! dq)
;;(rear-delete-deque! dq)

;;3.3.3 Representing Tables

;;1d
(defun make-table ()
  (list '*table*))

;;(setq tbl (make-table))

(defun assoc1 (key table)
  (cond ((null table) nil)
        ((eq key (caar table)) (car table))
        (:else (assoc1 key (cdr table)))))

;;(assoc1 'b (list (cons 'a 1) (cons 'b 2)))

(defun lookup (key table)
  (let ((record (assoc1 key table)))
    (if record
        (cdr record)
      nil)))

;;(lookup 'a (list (cons 'a 1) (cons 'b 2)))

(defun insert! (key value table)
  (let ((record (assoc1 key table)))
    (if record
        (setcdr record value)
      (setcdr table
              (cons (cons key value) (cdr table)))))
  table)

;;(insert! 'b 1 (list (cons 'a 2)))

;;2d

(defun look-up2 (key1 key2 table)
  (let ((subtable (assoc1 key1 table)))
    (if subtable
        (let ((record (assoc1 key2 subtable)))
          (if record
              (cdr record)
            nil))
      nil)))

(defun insert2! (key1 key2 value table)
  (let ((subtable (assoc1 key1 table)))
    (if subtable
        (let ((record (assoc1 key-2 (cdr subtable))))
          (if record
              (setcdr record value)
            (setcdr subtable
                    (cons (cons key2 value)
                          (cdr subtable)))))
      (setcdr table
              (cons (list key1
                          (cons key2 value))
                    (cdr table)))))
  table)

(defun make-table (same-key?)
  (letrec ((local-table (list (cons '*table* 'local-table)))
           (assoc1 (lambda (k table)
                     (cond
                      ((null table) nil)
                      ((funcall same-key? k (caar table)) (car table))
                      (:else (funcall assoc1 k (cdr table))))))
           (look-up (lambda (key1 key2)
                      (let ((subtable (assoc1 key1 local-table)))
                        (if subtable
                            (let ((record (assoc1 key2 (cdr subtable))))
                              (if record
                                  (cdr record)
                                nil))
                          nil))))
           (insert! (lambda (key1 key2 value)
                      (let ((subtable (assoc1 key1 local-table)))
                        (if subtable
                            (let ((record (assoc1 key2 (cdr subtable))))
                              (if record
                                  (setcdr record value)
                                (setcdr subtable
                                        (cons (cons key2 value)
                                              (cdr subtable)))))
                          (setcdr local-table
                                  (cons (list key1 (cons key2 value))
                                        (cdr local-table)))))))
           (dispatch (lambda (m)
                       (cond
                        ((eq m 'lookup-proc) look-up)
                        ((eq m 'insert-proc) insert!)
                        ((eq m 'value) local-table)
                        (:else (error "Unknown operation -- TABLE"))))))
    dispatch))

;;(fset 'ht (make-table 'eq))
;;(funcall (ht 'insert-proc) 'a 'b 1)
;;(ht 'value)
;;(funcall (ht 'insert-proc) 'a 'c 3)
;;(funcall (ht 'lookup-proc) 'a 'c)

;;ex3.24
;;included above

;;ex3.25

(defun make-table-multi ()
  (letrec ((local-table (list 'local-table))
           (look-up (lambda (keys)
                      (letrec ((iter (lambda (keys table)
                                       (let ((subtable (assoc1 (car keys) (cdr table))))
                                         (if subtable
                                             (if (null (cdr keys))
                                                 (cdr subtable)
                                               (funcall iter (cdr keys) subtable))
                                           false)))))
                        (funcall iter keys local-table))))
           (gen-map (lambda (keys value)
                      (cond
                       ((null (cdr keys)) (cons (car keys) value))
                       (:else (list (car keys) (funcall gen-map (cdr keys) value))))))
           (insert! (lambda (keys value)
                      (letrec ((iter (lambda (keys table)
                                       (let ((subtable (assoc1 (car keys) (cdr table))))
                                         (if subtable
                                             (if (null (cdr keys))
                                                 (setcdr subtable value)
                                               (funcall iter (cdr keys) subtable))
                                           (setcdr table (cons (funcall gen-map keys value)
                                                               (cdr table))))))))
                        (funcall iter keys local-table))))
           (dispatch (lambda (m)
                       (cond
                        ((eq m 'lookup-proc) look-up)
                        ((eq m 'insert-proc) insert!)
                        ((eq m 'value) local-table)
                        (:else (error "Unknown operation -- TABLE"))))))
    dispatch))

;;(fset 'ht-multi (make-table-multi))
;;(funcall (ht-multi 'insert-proc) (list 'a 'b 'd) 2)
;;(funcall (ht-multi 'insert-proc) (list 'a 'b 'c 'd) 3)
;;(funcall (ht-multi 'lookup-proc) (list 'a 'b))
;;(ht-multi 'value)

;;ex3.26

(defun entry (tree) (car tree))
(defun make-tree (entry-record left right) (list entry-record left right))
(defun left-branch (tree) (cadr tree))
(defun right-branch (tree) (caddr tree))

(defun adjoin-set (x set)
  (cond ((null set)  (make-tree x '() '()))
        ((eq (car x) (caar set)) (setcdr (car set) (cdr x)))
        ((< (car x) (caar set))
         (setcar (cdr set)
                 (cons (adjoin-set x left)
                       (right-branch set))))
        ((> (car x) (caar set))
         (setcar (cddr set)
                 (adjoin-set x right)))))

(defun assoc1 (key record-tree)
  (cond ((null record-tree) nil)
        ((eq key (car (entry record-tree))) (entry record-tree))
        ((< key (car (entry record-tree))) (assoc1 key (left-branch record-tree)))
        ((> key (car (entry record-tree))) (assoc1 key (right-branch record-tree)))))

;;(setq x (list (cons 1 2) '() ()))
;;(adjoin-set (cons 2 3) x)
;;(adjoin-set (cons 1 5) x)

;; simulator for digital circuits


(defun call-each (procedures)
  (if (null procedures)
      'done
    (progn
      (funcall (car procedures))
      (call-each (cdr procedures)))))

(defun make-wire ()
  (letrec ((signal-value 0)
           (action-procedures '())
           (set-my-signal! (lambda (new-value)
                             (if (not (= new-value signal-value))
                                 (progn (setq signal-value new-value)
                                        (call-each action-procedures)))))
           (accept-action-procedures! (lambda (proc)
                                        (setq action-procedures (cons proc action-procedures))
                                        (funcall proc)))
           (dispatch (lambda (m)
                       (cond ((eq m 'get-signal) signal-value)
                             ((eq m 'set-signal!) set-my-signal!)
                             ((eq m 'add-action!) accept-action-procedures!)
                             (:else (error "Unknown Operation -- WIRE"))))))
    dispatch))

(defun get-signal (wire)
  (funcall wire 'get-signal))

(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))

(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

;;(setq w (make-wire))
;;(get-signal w)
;;(set-signal! w 3)


(defun logical-not (s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (:else (error "Invalid Signal"))))

(defun inverter (input output)
  (let ((invert-input (lambda ()
                        (let ((new-value (logical-not (get-signal input))))
                          (after-delay inverter-delay
                                       (lambda ()
                                         (set-signal! output new-value)))))))
    (add-action! input invert-input)))

(defun logical-and (s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (:else 0)))

(defun and-gate (a1 a2 output)
  (let ((and-action-procedure (lambda ()
                                (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
                                  (after-delay and-gate-delay
                                               (lambda () (set-signal! output new-value)))))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok))

;;ex3.28

(defun logical-or (s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        (:else 0)))

(defun or-gate (a1 a2 output)
  (let ((or-action-procedure (lambda ()
                               (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
                                 (after-delay or-gate-delay
                                              (lambda () (set-signal! output new-value)))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

;;ex3.29

(defun or-gate2 (a1 a2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

;;ex3.30

(defun ripple-carry-adder (a-list b-list s-list c)
  (when (not (null a-list))
    (let ((carry (make-wire)))
      (full-adder (car a-list) (car b-list) c (car s-list) carry)
      (ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) carry))))


(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time1 the-agenda))
                  action
                  the-agenda))

(defun propagate ()
  (if (empty-agenda? the-agenda)
      'done
    (let ((first-item (first-agenda-item the-agenda)))
      (funcall first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(defun half-adder1 (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder1 (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder1 b c-in s c1)
    (half-adder1 a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defun make-time-segment (time queue) (cons time queue))
(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))

(defun make-agenda () (list 0))
(defun current-time1 (agenda) (car agenda))
(defun set-current-time! (agenda time) (setcar agenda time))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments) (setcdr agenda segments))
(defun first-segment (agenda) (car (segments agenda)))
(defun rest-segments (agenda) (cdr (segments agenda)))
(defun empty-agenda? (agenda) (null (segments agenda)))

(defun add-to-agenda! (time action agenda)
  (letrec ((belongs-before? (lambda (segments)
                              (or (null segments)
                                  (< time (segment-time (car segments))))))
           (make-new-time-segment (lambda (time action)
                                    (let ((q (make-queue)))
                                      (insert-queue! q action)
                                      (make-time-segment time q))))
           (add-to-segments! (lambda (segments)
                               (if (= time (segment-time (car segments)))
                                   (insert-queue! (segment-queue (car segments)) action)
                                 (let ((rest (cdr segments)))
                                   (if (funcall belongs-before? rest)
                                       (setcdr segments
                                               (cons (funcall make-new-time-segment time action)
                                                     (cdr segments)))
                                     (funcall add-to-segments! rest)))))))
    (let ((segments (segments agenda)))
      (if (funcall belongs-before? segments)
          (set-segments! agenda
                         (cons (funcall make-new-time-segment time action)
                               segments))
        (funcall add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(defun probe (name wire)
  (add-action! wire (lambda ()
                      (message "%s %d new-value = %s"
                               name
                               (current-time1 the-agenda)
                               (get-signal wire)))))

(setq the-agenda (make-agenda))
(defconst inverter-delay 2)
(defconst and-gate-delay 3)
(defconst or-gate-delay 5)
(setq input-1 (make-wire))
(setq input-2 (make-wire))
(setq sum1 (make-wire))
(setq carry1 (make-wire))
;;(probe 'sum sum1)
;;(probe 'carry carry1)
;;(half-adder1 input-1 input-2 sum1 carry1)
;;(set-signal! input-1 1)
;;(propagate)
;;(set-signal! input-2 1)

;; Propogation of Constraints

(defun for-each-except (exception procedure list)
  (letrec ((loop (lambda (items)
                (cond ((null items) 'done)
                      ((eq (car items) exception) (funcall loop (cdr items)))
                      (:else (funcall procedure (car items))
                             (funcall loop (cdr items)))))))
    (funcall loop list)))

(defun make-connector ()
  (letrec ((value 0)
           (informant nil)
           (constraints '())
           (set-my-value (lambda (newval setter)
                           (cond ((not (has-value? me))
                                  (setq value newval)
                                  (setq informant setter)
                                  (for-each-except setter
                                                   'inform-about-value
                                                   constraints))
                                 ((not (= value newval))
                                  (error "Contradiction"))
                                 (:else 'ignored))))
        (forget-my-value (lambda (retractor)
                           (if (eq retractor informant)
                               (progn
                                 (setq informant nil)
                                 (for-each-except retractor
                                                  'inform-about-no-value
                                                  constraints))
                             'ignored)))
        (connect (lambda (new-constraint)
                   (if (not (memq new-constraint constraints))
                       (setq constraints (cons new-constraint constraints)))
                   (if (has-value? me)
                       (inform-about-value new-constraint))
                   'done))
        (me (lambda (request)
              (cond ((eq request 'has-value?)
                     (if informant t nil))
                    ((eq request 'value) value)
                    ((eq request 'set-value!) set-my-value)
                    ((eq request 'forget!) forget-my-value)
                    ((eq request 'connect) connect)
                    (:else (error "Unknown operation -- CONNECTOR %s" request))))))
    me))

(defun has-value? (connector) (funcall connector 'has-value?))
(defun get-value (connector) (funcall connector 'value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector 'set-value!) new-value informant))
(defun forget-value! (connector retractor)
  (funcall (funcall connector 'forget!) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))

(defun adder (a1 a2 sum)
  (letrec ((process-new-value (lambda ()
                                (cond ((and (has-value? a1) (has-value? a2))
                                       (set-value! sum
                                                   (+ (get-value a1) (get-value a2))
                                                   me))
                                      ((and (has-value? a1) (has-value? sum))
                                       (set-value! a2
                                                   (- (get-value sum) (get-value a1))
                                                   me))
                                      ((and (has-value? a2) (has-value? sum))
                                       (set-value! a1
                                                   (- (get-value sum) (get-value a2))
                                                   me)))))
           (process-forget-value (lambda ()
                                   (forget-value! sum me)
                                   (forget-value! a1 me)
                                   (forget-value! a2 me)
                                   (funcall process-new-value)))
           (me (lambda (request)
                 (cond ((eq request 'i-have-a-value)
                        (funcall process-new-value))
                       ((eq request 'i-lost-my-value)
                        (funcall process-forget-value))
                       (:else (error "unknown Request -- ADDER %s" request))))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))

(defun inform-about-value (constraint)
  (funcall constraint 'i-have-a-value))

(defun inform-about-no-value (constraint)
  (funcall constraint 'i-lost-my-value))

(defun multiplier (m1 m2 product)
  (letrec ((process-new-value (lambda ()
                                (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                                           (and (has-value? m2) (= (get-value m2) 0)))
                                       (set-value! product 0 me))
                                      ((and (has-value? m1) (has-value? m2))
                                       (set-value! product
                                                   (* (get-value m1) (get-value m2))
                                                   me))
                                      ((and (has-value? m1) (has-value? product))
                                       (set-value! m2
                                                   (/ (get-value product) (get-value m1))
                                                   me))
                                      ((and (has-value? m2) (has-value? product))
                                       (set-value! m1
                                                   (/ (get-value product) (get-value m2))
                                                   me)))))
           (process-forget-value (lambda ()
                                   (forget-value! product me)
                                   (forget-value! m1 me)
                                   (forget-value! m2 me)
                                   (funcall process-new-value)))
           (me (lambda (request)
                 (cond ((eq request 'i-have-a-value)
                        (funcall process-new-value))
                       ((eq request 'i-lost-my-value)
                        (funcall process-forget-value))
                       (:else (error "unknown Request -- MULTIPLIER %s" request))))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))

(defun constant (value connector)
  (letrec ((me (lambda (request)
                 (error "Unknown request -- CONSTANT %s" request))))
    (connect connector me)
    (set-value! connector value me)
    me))

(defun probe (name connector)
  (letrec ((print-probe (lambda (value)
                          (message "Probe: %s = %s" name value)))
           (process-new-value (lambda ()
                                   (funcall print-probe (get-value connector))))
           (process-forget-value (lambda ()
                                   (funcall print-probe "?")))
           (me (lambda (request)
                 (cond ((eq request 'i-have-a-value)
                        (funcall process-new-value))
                       ((eq request 'i-lost-my-value)
                        (funcall process-forget-value))
                       (:else
                        (error "Unknown request -- PROBE %s" request))))))
    (connect connector me)
    me))

(defun celcius-fahrenheit-convertor (c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;;(setq C (make-connector))
;;(setq F (make-connector))
;;(celcius-fahrenheit-convertor  C F)
;;(probe "Celcius Temp" C)
;;(probe "Fahrenheit Temp" F)
;;(set-value! C 25 'user)
;;(set-value! F 212 'user)
;;(forget-value! F 'user)

;;ex3.33

(defun averager (a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (adder a b d)
    (constant 2 e)
    (multiplier c e d)))

;;(setq a (make-connector))
;;(setq b (make-connector))
;;(setq c (make-connector))
;;(probe "First Value" a)
;;(probe "Second Value" b)
;;(probe "Averager" c)
;;(averager a b c)
;;(set-value! a 3 'user)
;;(set-value! c 5 'user)

;;ex3.35

(defun squarer (a b)
  (letrec ((process-new-value (lambda ()
                                (cond 
                                 ((has-value? a)
                                       (set-value! b
                                                   (expt (get-value a) 2)
                                                   me))
                                 ((has-value? b) 
                                       (set-value! a
                                                   (expt (get-value b) 0.5)
                                                   me)))))
           (process-forget-value (lambda ()
                                   (forget-value! b me)
                                   (forget-value! a me)
                                   (funcall process-new-value)))
           (me (lambda (request)
                 (cond ((eq request 'i-have-a-value)
                        (funcall process-new-value))
                       ((eq request 'i-lost-my-value)
                        (funcall process-forget-value))
                       (:else (error "unknown Request -- MULTIPLIER %s" request))))))
    (connect a me)
    (connect b me)
    me))

;;(setq a (make-connector))
;;(setq b (make-connector))
;;(probe "Root" a)
;;(probe "Square" b)
;;(squarer a b)
;;(set-value! a 3 'user)
;;(forget-value! a 'user)
;;(set-value! b 5 'user)

;;ex3.37

(defun c+ (x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(defun c- (x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(defun c/ (x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(defun cv (x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(defun celsius-fahrenheit-convertor2 (x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

;;(setq cel (make-connector))
;;(setq far (celsius-fahrenheit-convertor2 cel))

;;Streams

(defun memo-proc (proc)
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if (not already-run?)
          (progn (setq result (funcall proc))
                 (setq already-run? t)
                 result)
        result))))

;;(fset 'mem2 (memo-proc (lambda ()  2)))
;;(mem2)

(defmacro delay (proc)
  `(memo-proc (lambda () ,proc)))

;;(force (delay 2))

(defun force (proc)
  (funcall proc))

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

(defun car-stream (s)
  (car s))

(defun cdr-stream (s)
  (force (cdr s)))

(defun stream-null? (s)
  (null s))

(defconst the-empty-stream '())

(defun stream-ref (s n)
  (if (= n 0)
      (car-stream s)
    (stream-ref (cdr-stream s) (- n 1))))

(defun stream-map (proc s)
  (if (stream-null? s)
      the-empty-stream
    (cons-stream (funcall proc (car-stream s))
                 (stream-map proc (cdr-stream s)))))

(defun stream-filter (proc s)
  (if (stream-null? s)
      the-empty-stream
    (if (funcall proc (car-stream s))
        (cons-stream (car-stream s)
                     (stream-filter proc (cdr-stream s)))
      (stream-filter proc (cdr-stream s)))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      the-empty-stream
    (progn (funcall proc (car-stream s))
           (stream-for-each proc (cdr-stream s)))))

(defun display-line (x)
  (print x)
  (newline))

(defun display-stream (s)
  (stream-for-each 'display-line s))

;;(display-stream (cons-stream 1 (cons-stream 2 nil)))

(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

(defun second-odd (l h)
  (car-stream (cdr-stream (stream-filter 'oddp (stream-enumerate-interval l h)))))

;;(second-odd 4 2000000000)

;;ex3.50

(defun stream-map-multi (proc args)
  (if (stream-null? (car args))
      the-empty-stream
    (cons-stream
     (apply proc (map 'list 'car-stream args))
     (stream-map-multi proc (map 'list 'cdr-stream args)))))

;;(display-stream (stream-map-multi '+ (list (stream-enumerate-interval 1 5) (stream-enumerate-interval 1 20))))

;;ex3.51

(defun show (x)
  (display-line x)
  x)

;;(defconst x (stream-map 'show (stream-enumerate-interval 0 10))) -> 0
;;(stream-ref x 5) -> 1 2 3 4 5
;;(stream-ref x 7) -> 6 7

;;ex3.52

;; (setq sum 0) -> 0

(defun accum (x)
  (setq sum (+ x sum))
  sum)

;; (setq seq (stream-map 'accum (stream-enumerate-interval 1 20))) -> 1
;; (setq y (stream-filter 'evenp seq)) -> 6
;; (setq z (stream-filter (lambda (x) (= (mod x 5) 0)) seq)) -> 10
;; (stream-ref y 7) -> 136
;; (display-stream z) -> 10 15 45 55 105 120 190 210

(defun divisible? (a b)
  (= (mod a b) 0))

(defun integers-starting-from (n)
  (cons-stream n (integers-starting-from (+ n 1))))

(defun sieve (stream)
  (cons-stream
   (car-stream stream)
   (sieve
    (stream-filter (lambda (x)
                     (not (divisible? x (car-stream stream))))
                   (cdr-stream stream)))))

(defconst primes (sieve (integers-starting-from 2)))
;;(stream-ref primes 50) -> 233

(defconst ones (cons-stream 1 ones))

(defun add-streams (s1 s2)
  (stream-map-multi '+ (list s1 s2)))

(defconst integers (cons-stream 1 (add-streams ones integers)))

(defconst fibs (cons-stream 0
                            (cons-stream 1
                                         (add-streams (cdr-stream fibs)
                                                      fibs))))

;(stream-ref fibs 10) -> 55

(defun scale-stream (stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(defconst double (cons-stream 1 (scale-stream double 2)))
;;(stream-ref double 10) -> 1024


(defconst primes2
  (cons-stream 2
               (stream-filter 'prime? (integers-starting-from 3))))

(defun prime? (n)
  (letrec ((iter (lambda (ps)
                (cond ((> (square (car-stream ps)) n) t)
                      ((divisible? n (car-stream ps)) nil)
                      (:else (funcall iter (cdr-stream ps)))))))
    (funcall iter primes2)))
;;(stream-ref primes2 50) -> 233

;;ex3.54

(defun mul-streams (s1 s2)
  (stream-map-multi '* (list s1 s2)))

(defconst factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))
;;(stream-ref factorials 3) -> 24

;;ex3.55

(defun partial-sums (s)
  (add-streams s
               (cons-stream 0
                            (partial-sums s))))

;;(stream-ref (partial-sums (integers-starting-from 1)) 4) -> 15

;;ex3.56

(defun stream-take (s n)
  (if (= n 0)
     nil 
    (cons (car-stream s)
          (stream-take (cdr-stream s) (- n 1)))))

;;(stream-take integers 10) -> (1 2 3 4 5 6 7 8 9 10)

(defun merge-streams (s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (:else (let ((s1car (car-stream s1))
                     (s2car (car-stream s2)))
                 (cond ((< s1car s2car) (cons-stream s1car
                                                     (merge-streams (cdr-stream s1)
                                                            s2)))
                       ((> s1car s2car) (cons-stream s2car
                                                     (merge-streams s1
                                                            (cdr-stream s2))))
                       (:else (cons-stream s1car
                                           (merge-streams (cdr-stream s1)
                                                  (cdr-stream s2)))))))))

(defconst S (cons-stream 1 (merge-streams (scale-stream integers 2)
                                (merge-streams (scale-stream integers 3)
                                       (scale-stream integers 5)))))

;;(stream-take S 10) -> (1 2 3 4 5 6 8 9 10 12)

;;ex3.58

(defun expand (num den radix)
  (cons-stream
   (/ (* num radix) den)
   (expand (% (* num radix) den) den radix)))

;;(stream-take (expand 1 7 10) 12)-> (1 4 2 8 5 7 1 4 2 8 5 7)
;;(stream-take (expand 3 8 10) 10) -> (3 7 5 0 0 0 0 0 0 0)

;;ex3.59

(defun div-streams (s1 s2)
  (stream-map-multi '/ (list s1 (scale-stream s2 1.0))))

(defun integrate-series (s)
  (div-streams s integers))

;;(stream-take (div-streams integers integers) 10) -> (1 1 1 1 1 1 1 1 1 1)

(defconst exp-series
  (cons-stream 1 (integrate-series exp-series)))

;;(stream-take exp-series 10)

(defconst cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(defconst sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;(stream-take cosine-series 5)
;;(stream-take sine-series 6)

;;ex3.60

(defun mul-series (s1 s2)
  (cons-stream (* (car-stream s1) (car-stream s2))
               (add-streams (add-streams (scale-stream (cdr-stream s1) (car-stream s2))
                                         (scale-stream (cdr-stream s2) (car-stream s1)))
                            (mul-series (cdr-stream s1) (cdr-stream s2)))))

;;(stream-take (partial-sums (add-streams (mul-series cosine-series cosine-series) (mul-series sine-series sine-series))) 10)

;;ex3.61

(defun invert-unit-series (s)
  (cons-stream 1
               (scale-stream
                (mul-series (cdr-stream s)
                            (invert-unit-series s))
                -1)))

;; (defconst secant-series (invert-unit-series cosine-series))
;;(stream-take secant-series 10)
;;(stream-take (mul-series cosine-series secant-series) 10)

;;ex3.62

(defun div-series (s1 s2)
  (let ((c (car-stream s2)))
    (if (= 0 c)
        (error "Denominator series has zero constant term -- DIV-SERIES")
      (mul-series s1
                  (scale-stream (invert-unit-series (scale-stream s2
                                                                  (/ 1.0 c)))
                                (/ 1.0 c))))))

(defconst tangent-series (div-series sine-series cosine-series))
;;(stream-take tangent-series 10)

(defun average (a b)
  (/ (+ a b) 2))

(defun sqrt-improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-stream (x)
  (letrec ((guesses (cons-stream 1.0
                       (stream-map (lambda (guess)
                                     (sqrt-improve guess x))
                                   guesses))))
    guesses))

;;(stream-take (sqrt-stream 2) 10)

(defun pi-summands (n)
  (cons-stream (/ 1.0 n)
               (stream-map '- (pi-summands (+ n 2)))))

;;(stream-take (pi-summands 1) 10)

(defconst pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

;;(stream-take pi-stream 10)

;;Sequence Accelerators
;; https://en.wikipedia.org/wiki/Series_acceleration

(defun euler-transform (s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (cdr-stream s)))))

;;(stream-take (euler-transform pi-stream) 10)

(defun make-tableau (transform s)
  (cons-stream s
               (make-tableau transform
                             (funcall transform s))))

(defun accelerated-sequence (transform s)
  (stream-map 'car-stream
              (make-tableau transform s)))

;;(stream-take (accelerated-sequence 'euler-transform pi-stream) 10)

;;ex3.64

(defun stream-limit (s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0)) tolerance)
        s1
      (stream-limit (cdr-stream s) tolerance))))

(defun sqrt (x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;(sqrt 2 0.000001)

;;ex3.65

(defun ln-summands (n)
  (cons-stream (/ 1.0 n)
               (stream-map '- (ln-summands (+ n 1)))))

(defconst ln-stream (partial-sums (ln-summands 1)))
;;(stream-take ln-stream 10)
;;(stream-take (accelerated-sequence 'euler-transform ln-stream) 10)

(defun interleave (s1 s2)
  (if (stream-null? s1)
      s2
    (cons-stream (car-stream s1)
                 (interleave s2 (cdr-stream s1)))))

(defun pairs (s t)
  (cons-stream
   (list (car-stream s) (car-stream t))
   (interleave
    (stream-map (lambda (x) (list (car-stream s) x))
                (cdr-stream t))
    (pairs (cdr-stream s) (cdr-stream t)))))

;;(stream-take (pairs integers integers) 10)

;;ex3.66

(defun stream-index (s item)
  (letrec ((iter (lambda (s index)
                    (if (equal item (car-stream s))
                        index
                      (funcall iter (cdr-stream s) (+ index 1))))))
    (funcall iter s 1)))

;;(stream-index (pairs integers integers) (list 1 100)) -> 198

;;ex3.67

(defun pairs-full (s t)
  (cons-stream
   (list (car-stream s) (car-stream t))
   (interleave
    (interleave (stream-map (lambda (x) (list (car-stream s) x))
                            (cdr-stream t))
                (stream-map (lambda (y) (list y (car-stream t)))
                            (cdr-stream s)))
    (pairs-full (cdr-stream s) (cdr-stream t)))))

;;(stream-take (pairs-full integers integers) 10)

;;ex3.68

(defun loose-pairs (s t)
  (interleave
   (stream-map (lambda (x) (list (car-stream s) x))
               t)
   (loose-pairs (cdr-stream s) (cdr-stream t))))
;;(stream-take (loose-pairs integers integers) 10) -> infinite loop

;;ex3.69

(defun triples (s t u)
  (let ((tu-pairs (pairs t u)))
    (cons-stream
     (list (car-stream s) (car-stream t) (car-stream u))
     (interleave
      (stream-map (lambda (x) (cons (car-stream s) x))
                  tu-pairs)
      (triples (cdr-stream s) (cdr-stream t) (cdr-stream u))))))

;;(stream-take (triples integers integers integers) 10)

(defun pythagorean? (a b c) 
   (= (square c) 
      (+ (square a) (square b))))

(defconst pythagorean-triples
  (stream-filter
   (lambda (triplet)
     (apply 'pythagorean? triplet))
   (triples integers integers integers)))

;;(stream-take pythagorean-triples 1) -> ((3 4 5))


;;ex3.70

(defun merge-weighted-streams (s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (:else (let ((s1car (car-stream s1))
                     (s2car (car-stream s2)))
                 (cond ((< (funcall weight s1car)
                           (funcall weight s2car))
                        (cons-stream s1car
                                     (merge-weighted-streams (cdr-stream s1)
                                                             s2
                                                             weight)))
                       (:else
                        (cons-stream s2car
                                     (merge-weighted-streams s1
                                                             (cdr-stream s2)
                                                             weight))))))))

(defun weighted-pairs (s t weight)
  (cons-stream
   (list (car-stream s) (car-stream t))
   (merge-weighted-streams
    (stream-map (lambda (x) (list (car-stream s) x))
                (cdr-stream t))
    (weighted-pairs (cdr-stream s) (cdr-stream t) weight)
    weight)))

(defconst a (weighted-pairs integers integers (lambda (x) (apply '+ x))))
;;(stream-take a 10)

(defconst h (stream-filter (lambda (x) (not
                                        (or (= (mod x 2) 0)
                                            (= (mod x 3) 0)
                                            (= (mod x 5) 0)))) integers))
;;(stream-take h 10)
(defconst b (weighted-pairs h h (lambda (pair)
                                  (+ (* 2 (car pair))
                                     (* 3 (cadr pair))
                                     (* 5 (car pair) (cadr pair))))))
;;(stream-take b 10)

;;ex3.71

(defun triple (x) (* x x x)) 
(defun sum-triple (x) (+ (triple (car x)) (triple (cadr x))))

(defconst r (weighted-pairs integers integers 'sum-triple))
;;(stream-take r 10)
(stream-index r (list 1 12))
(stream-index r (list 9 10))

(defun ramanujan (s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (= (sum-triple s0) (sum-triple s1))
        (cons-stream (sum-triple s0)
                     (ramanujan (cdr-stream s)))
      (ramanujan (cdr-stream s)))))

;;(stream-take (ramanujan r) 6) -> (1729 4104 13832 20683 32832 39312)

;;ex3.72

(defun square (x) (* x x))
(defun sum-square (x) (+ (square (car x)) (square (cadr x))))
(defconst w (weighted-pairs integers integers 'sum-square))

(defun ramanujan2 (s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (if (= (sum-square s0) (sum-square s1) (sum-square s2))
        (cons-stream (list (sum-square s0) s0 s1 s2)
                     (ramanujan2 (cdr-stream s)))
      (ramanujan2 (cdr-stream s)))))

;;(stream-take (ramanujan2 w) 6)->((325 (10 15) (6 17) (1 18)) (425 (13 16) (8 19) (5 20)) (650 (17 19) (11 23) (5 25)) (725 (14 23) (10 25) (7 26)) (845 (19 22) (13 26) (2 29))))

;;Streams as signals

(defun integral (integrand intial-value dt)
  (letrec ((int (cons-stream initial-value
                             (add-streams (scale-stream integrand dt)
                                          int))))
    int))

;;ex3.73

(defun rc (r c dt)
  (lambda (i v0)
    (add-streams
     (integral (scale-stream i (/ 1.0 c)) v0 dt)
     (scale-stream i r))))

;;(defconst rc1 (rc 5 1 0.5))

;;ex3.74

(defun sign-change-detector (b a)
  (cond ((and (< a 0) (>= b 0)) 1)
        ((and (>= a 0) (< b 0)) -1)
        (:else 0)))

(defun make-zero-crossings (input-stream last-value)
  (cons-stream
   (sign-change-detector (car-stream input-stream) last-value)
   (make-zero-crossings (cdr-stream input-stream)
                        (car-stream input-stream))))

(defun zero-crossings (sense-data) (make-zero-crossings sense-data 0))

(defun zero-crossings2 (sense-data)
  (stream-map-multi 'sign-change-detector
                    sense-data
                    (cons-stream 0 sense-data)))

;;ex3.75

(defun make-zero-crossings2 (input-stream last-value last-avpt)
  (let ((avpt (/ (+ (car-stream input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings2 (cdr-stream input-stream)
                                       (car-stream input-stream)
                                       avpt))))

;;ex3.76

(defun smooth (input-stream)
  (stream-map 'average input-stream (cdr-stream input-stream)))

(defun zero-crossings3 (input-stream)
  (zero-crossings2 (smooth input-stream)))

;;Streams and Delayed Evaluation

(defun integral2 (delayed-integrand initial-value dt)
  (letrec ((int (cons-stream initial-value
                             (let ((integrand (force delayed-integrand)))
                                 (add-streams (scale-stream integrand dt)
                                               int)))))
    int))

(defun solve (f y0 dt)
  (letrec ((y (integral2 (delay dy) y0 dt))
           (dy (stream-map f y)))
    y))

;;(stream-ref (solve (lambda (y) y) 1 0.001) 1000) -> 2.716923932235896

;;ex3.77

(defun integral3 (delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                             the-empty-stream
                   (integral3 (delay (cdr-stream integrand))
                              (+ (* dt (car-stream integrand))
                                 initial-value)
                              dt)))))

;;ex3.78

(defun solve-2nd (a b dt y0 dy0)
  (letrec ((y (integral2 (delay dy) y0 dt))
           (dy (integral2 (delay ddy) y0 dt))
           (ddy (add-streams
                 (scale-stream dy a)
                 (scale-stream y b))))
    y))

;;(stream-ref (solve-2nd 1 0 0.002 1 1) 500)- >2.715568520651728(~e)

;;ex3.79

(defun solve-2nd-gen (f dt y0 dy0)
  (letrec ((y (integral2 (delay dy) y0 dt))
           (dy (integral2 (delay ddy) y0 dt))
           (ddy (stream-map-multi dy y)))
    y))

;;ex3.80

(defun rlc (r l c dt)
  (letrec ((proc (lambda (vc0 il0)
                   (letrec ((vc (scale-stream (integral2 (delay il) vc0 dt)
                                              (/ -1 c)))
                            (il (integral2 (delay dil) il0 dt))
                            (dil (add-streams (scale-stream vc (/ 1.0 l))
                                              (scale-stream il (/ (- r) c)))))
                     (stream-map-multi 'cons (list vc il))))))
    proc))

(fset 'rlc1 (rlc 1 1 0.2 0.1))
;;(stream-take (rlc1 0 10) 10)

;; Modularity

(defconst random-numbers
  (cons-stream random-init
               (stream-map 'rand-update random-numbers)))

;;(stream-take random-numbers 10)

(defun map-succesive-pairs (f s)
  (cons-stream
   (funcall f (car-stream s) (car-stream (cdr-stream s)))
   (map-succesive-pairs f (cdr-stream (cdr-stream s)))))

(defconst cesaro-stream
  (map-succesive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                       random-numbers))
;;(stream-take cesaro-stream 10)

(defun monte-carlo2 (experiment-stream passed failed)
  (letrec ((next (lambda (passed failed)
                   (cons-stream (/ (* 1.0 passed) (+ passed failed))
                                (monte-carlo2 (cdr-stream experiment-stream)
                                             passed
                                             failed)))))
    (if (car-stream experiment-stream)
        (funcall next (+ passed 1) failed)
      (funcall next passed (+ failed 1)))))

;;(stream-take (monte-carlo2 cesaro-stream 0 0) 10)
(defconst pi (stream-map (lambda (p) (expt (/ 6.0 p) 0.5))
                         (monte-carlo2 cesaro-stream 1 0)))

;;(stream-ref pi 200) -> 3.232379288089343

;;ex3.81

(defun rand-stream (request-stream)
  (cons-stream random-init
               (stream-map-multi (lambda (req prev)
                                   (if (eq req 'generate)
                                       (rand-update prev)
                                       (cdr req)))
                                 (list request-stream
                                       random-numbers))))

(defconst req-stream  
  (cons-stream  
   (cons 'reset 12) 
   (cons-stream 'generate 
                (cons-stream (cons 'reset 100) 
                             (cons-stream 'generate 
                                          gen-stream)))))

;;(stream-take (rand-stream req-stream) 10)

;;ex3.82

(defun estimate-integral-stream (predicate? x1 x2 y1 y2)
  (let* ((experiment-stream (stream-map (lambda (_)
                                          (funcall predicate?
                                                   (random-in-range x1 x2)
                                                   (random-in-range y1 y2)))
                                        ones))
         (prob-stream (monte-carlo2 experiment-stream 0 0)))
    (stream-map
     (lambda (p) (/ (* p (abs (- x1 x2)) (abs (- y1 y2))) 9))
     prob-stream)))

;;(let ((max-lisp-eval-depth 10000) (max-specpdl-size 10000)) (stream-ref (estimate-integral-stream 'lies-in-circle? 2 8 4 10) 1000)) -> 3.0489510489510487
