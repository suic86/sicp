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
(test 0 (p))          
