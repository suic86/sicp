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
