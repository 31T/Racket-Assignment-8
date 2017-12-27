;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ca) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 09, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Part A

;;(apply-rule a b c r) consumes a rule number r and three numbers (a, b, c) representing
;; white and black squares and produces the result of applying rule r to a, b, and c
;;apply-rule: Nat Nat Nat Nat -> Nat
;;requires: a, b, and c are either 0 or 1
;;          0 <= r <= 255
;;Examples:
(check-expect (apply-rule 1 1 1 86) 0)
(check-expect (apply-rule 0 0 0 86) 0)

(define (apply-rule a b c r)
  (cond
    [(odd? (floor (/ r (expt 2 (+ (* a 4) (* b 2) c))))) 1]
    [else 0]))

;;Tests:
(check-expect (apply-rule 1 1 0 86) 1)
(check-expect (apply-rule 1 0 1 24) 0)


;;Part B

;;(next-row lon r) consumes a list of 0s and 1s (lon) and a rule number (r) and produces
;; the next row that results from applying rule r to lon
;;next-row: (listof (anyof 0 1)) Nat -> (listof (anyof 0 1)) Nat
;;requires: lon is at least of length 1
;;          0 <= r <= 255
;;Examples:
(check-expect (next-row (list 1) 86) (list 1))
(check-expect (next-row (list 1 1) 86) (list 0 1))
(check-expect (next-row (list 0 1 0) 45) (list 0 1 1))

(define (next-row lon r)
  (local
    [(define (build-row i n lon r)
       (cond
         [(= 0 n) (list (apply-rule 0 (first lon) 0 r))]
         [(= 1 n) (list (apply-rule 0 (first lon) (second lon) r)
                        (apply-rule (first lon) (second lon) 0 r))]
         [(= i 0) (append (list (apply-rule 0 (first lon) (second lon) r))
                          (build-row (add1 i) n lon r))]
         [(= i (sub1 n)) (list (apply-rule (first lon) (second lon) (third lon) r)
                               (apply-rule (first lon) (second lon) 0 r))]
         [else (append (list (apply-rule (first lon) (second lon) (third lon) r))
                       (build-row (add1 i) n (rest lon) r))]))]
    (build-row 0 (sub1 (length lon)) lon r)))

;;Tests:
(check-expect (next-row (list 1 0 1) 86) (list 1 0 1))
(check-expect (next-row (list 1 1 1 1 1 1 1 1 0 1) 50)
              (list 0 0 0 0 0 0 0 0 1 1))


;;Part C

;;(iterate f b n) consumes a function f, a base value b and a number n and produces
;; a list of length n where the first element is b and every other element results
;; from appyling f to the previous element
;;iterate: (X -> Y) X Nat -> (listof (anyof X Y))
;;Examples:
(check-expect (iterate sqr 2 4) (list 2 4 16 256))
(check-expect (iterate add1 1 1) (list 1))

(define (iterate f b n)
  (cond
    [(= n 1) (list b)]
    [else (append (list b) (iterate f (f b) (sub1 n)))]))

;;Tests:
(check-expect (iterate add1 2 2) (list 2 3))
(check-expect (iterate floor 5.2 2) (list 5.2 5))


;;Part D

;;(run-automaton lon r n) consumes a row of 0s and 1s (lon), a rule number (r) and
;; a number of generations (n), and produces a list of n lists, where the first list
;; is lon and each subsequent row is the result of applying r to the row before it
;;run-automaton: (listof (anyof 0 1)) Nat Nat -> (listof (anyof 0 1))
;;requires: 0 <= r <= 255
;;          lon is non-empty
;;          n > 0
;;Examples:
(check-expect (run-automaton (list 1) 86 4) (list (list 1) (list 1) (list 1) (list 1)))
(check-expect (run-automaton (list 1 0) 2 3) (list (list 1 0) (list 0 0) (list 0 0)))
(check-expect (run-automaton (list 1 0 1 0 1) 86 2)
              (list (list 1 0 1 0 1) (list 1 0 1 0 1)))

(define (run-automaton lon r n)
  (iterate (lambda (x) (next-row x r)) lon n))

;;Tests:
(check-expect (run-automaton (list 1 1 1) 56 1) (list (list 1 1 1)))
(check-expect (run-automaton (list 1 1 1) 84 2) (list (list 1 1 1) (list 0 0 1)))
(check-expect (run-automaton (list 1 0 1) 56 4)
              (list (list 1 0 1) (list 0 1 1) (list 0 1 0) (list 0 0 0)))


