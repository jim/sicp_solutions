; SICP Exercise 1.7

(define (puts x)
  (display x)
  (newline))

(define (square x)
  (* x x ))

(define (sqrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
    guess
    (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? last-guess guess)
  (< (/ (abs (- last-guess guess)) guess) 0.001))

(define (sqrt x)
  (sqrt-iter 0 1.0 x))

; basic
(puts (sqrt 25))

; small number, was inaccurate with original method
(puts (sqrt .0000025))

; large number, would hang with original method
(puts (sqrt 250000000000000))
