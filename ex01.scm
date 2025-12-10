;; -*- geiser-scheme-implementation: guile -*-

;; Exercise 1.1

10                                   ;; 10
(+ 5 3 4)                            ;; 12
(- 9 1)                              ;; 8
(/ 6 2)                              ;; 3
(+ (* 2 4) (- 4 6))                  ;; 6
(define a 3)                         ;; a = 3
(define b (+ a 1))                   ;; b = 4
(+ a b (* a b))                      ;; 19
(if (and (> b a) (< b (* a b))) b a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                     ;; 16
(+ 2 (if (> b a) b a))               ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                          ;; 16

;; Exercise 1.2

;; (5 + 4 + (2 - (3 - (6 + 4/5)))) / (3 * (6 - 2) * (2 - 7))
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ;; -37/150

;; Exercise 1.3

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (max x y) (if (> x y) x y))
(define (min x y) (if (> x y) y x))
(define (sum-of-square-two-larger x y z)
  (sum-of-squares (max x y) (max (min x y) z)))

;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;; infinite loop
;; (test 0 (p))

;; Example 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x) x)))

;; Exercise 1.7
;;
;; Skipped for now.

;; Exercise 1.8

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

(define (cbrt-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cbrt-iter (improve-cube guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

;; Exercise 1.9

(define (inc n) (- n (- 0 1)))
(define (dec n) (- n 1))

;; recursive procedure and recursive process
;; (define (+ a b)
;;   (if (= a 0) b (inc (+ (dec a) b))))

;; recursive procedure and iterative process
;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b))))

;; Exercise 1.10 - Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; (= (A 1 10) 1024)
;; (= (A 2 4) 65536)
;; (= (A 3 3) 65536)

(define (f n) (A 0 n))    ;; f(n) = 2n
(define (g n) (A 1 n))    ;; g(n) = 2 ** n
(define (h n) (A 2 n))    ;; h(n) = 2 \uparrow\uparrow n
(define (k n) (* 5 n n))  ;; k(n) = 5n^2

;; Fibonacci - tree recursive

(define (fib-recur n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recur (- n 1))
                 (fib-recur (- n 2))))))

;; Fibonacci - iterative

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 n))

;; Example: Counting change

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Exercise 1.11

(define (fun-rec n)
  (if (< n 3)
      n
      (+ (fun-rec (- n 1))
         (* 2 (fun-rec (- n 2)))
         (* 3 (fun-rec (- n 3))))))


(define (fun n)
  (define (fun-iter m c b a)
    (if (= n m) c
        (fun-iter (+ m 1) (+ (* 3 a) (* 2 b) c) c b)))
  (if (< n 3) n
      (fun-iter 2 2 1 0)))

;; Exercise 1.12

(define (n-over-k n k)
  (if (> k n)
      (throw 'invalid-argument "n must be less than equal to k")
      (cond ((= n 0) 1)
            ((= k 0) 1)
            ((= k n) 1)
            (else (+
                   (n-over-k (- n 1) k)
                   (n-over-k (- n 1) (- k 1)))))))

;; Exercise 1.13
;;
;; Skipped for now.
