; SICP Exercise 1.3
;
; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define assert-equal
  (lambda (expected actual message)
    (let ([color (lambda (str color)
                   (string-append 
                     (string (integer->char 27)) "[" color "m" str 
                     (string (integer->char 27)) "[0m"))])
      (let ([red (lambda (str) (color str "31"))]
            [green (lambda (str) (color str "32"))]
            [bold (lambda (str) (color str "1"))])
        (print message ": " (if (equal? expected actual)
                                (green "pass")
                                (string-append (red "FAIL")
                                               " expected "
                                               (bold (object->string expected))
                                               ", but recieved "
                                               (bold (object->string actual)))))
        (newline)))))

(define largest-in-list
  (lambda (l)
    (cond ((= 0 (length l)) #f)
          ((= 1 (length l)) (car l))
          (else (let ([first (car l)]
                      [rest (largest-in-list (cdr l))])
                      (if (> first rest) first rest))))))

(define list-without
  (lambda (l datum)
    (cond
      ((= (length l) 0) l)
      ((equal? datum (car l)) (list-without (cdr l) datum))
      (else (cons (car l) (list-without (cdr l) datum))))))

(assert-equal 1 (largest-in-list '(1)) "1 is largest of (1)")
(assert-equal 2 (largest-in-list '(2 1)) "2 is largest of (2 1)")
(assert-equal 4 (largest-in-list '(2 3 4)) "4 is largest of (2 3 4)")

(assert-equal '(2) (list-without '(1 2) 1) "(1 2) without 1 is (2)")
(assert-equal '(3 4) (list-without '(2 3 4) 2) "(2 3 4) without 2 is (2 4)")

; Solution

(define (sum-squares a b c)
  (let ([square (lambda (x) (* x x))]
        [args (list a b c)])
    (let ([largest-number (largest-in-list args)])
      (let ([second-largest-number (largest-in-list (list-without args largest-number))])
        (+ (square largest-number) (square second-largest-number))))))

(assert-equal 25 (sum-squares 2 3 4) "25 is the sum of 3^2 and 4^2")
(assert-equal 8469 (sum-squares 87 14 30) "8469 is the sum of 87^2 and 30^2")
(assert-equal 637 (sum-squares 21 14 7) "637 is the sum of 21^2 and 14^2")

