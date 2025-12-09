(define (factorial n)
  (define (factorial-iter total counter)
    (if (= counter n)
        total
        (factorial-iter (* total counter) (+ counter 1))))
  (factorial-iter 1 1))
