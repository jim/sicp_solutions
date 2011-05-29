; SICP Exercise 1.8

(define (puts x)
  (display x)
  (newline))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-iter last-guess guess x)
  (if (good-enough? last-guess guess)
    guess
    (cube-iter guess (improve guess x) x)))

(define (improve guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (good-enough? last-guess guess)
  (< (/ (abs (- last-guess guess)) guess) 0.001))

(define (cube-root x)
  (cube-iter 0 1.0 x))

; basic
(puts (cube-root 27))
