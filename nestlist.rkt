;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 08, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Part A

;;(nfoldr comb1 comb2 base lst) consumes a comb1, comb2, base and lst and applies
;; the comb1 function if the first element of lst is of type X and the comb2 function if
;; the first element of lst is a list, produces base if lst is empty and produces
;; the result of applying comb1 and/or comb2 and base to lst if lst is not empty
;;nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;;Examples:
(check-expect (nfoldr cons cons empty empty) empty)
(check-expect
 (nfoldr (lambda (x y) (+ x y)) (lambda (x y) (* x y)) 0 (list (list 1) 2)) 2)

(define (nfoldr comb1 comb2 base lst)
  (cond
    [(empty? lst) base]
    [(list? (first lst))
     (comb2 (nfoldr comb1 comb2 base (first lst))
            (nfoldr comb1 comb2 base (rest lst)))]
    [else (comb1 (first lst)
                 (nfoldr comb1 comb2 base (rest lst)))]))

;;Tests:
(check-expect
 (nfoldr (lambda (x y) (+ x y)) (lambda (x y) (* x y)) 1 (list (list 2) 4)) 15)
(check-expect (nfoldr (lambda (x y) (+ x y)) (lambda (x y) (* x y)) 3.14 empty) 3.14)


;;Part B

;;(nfilter f lst) applies the consumed function f to each of the elements in lst and
;; produces a nested list of elements in lst that produce true when applied with f
;;nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (nfilter odd? '(1 (2 3) () ((4)))) '(1 (3) () (())))
(check-expect (nfilter odd? empty) empty)

(define (nfilter f lst)
  (nfoldr (lambda (frst rst) (cond
                               [(f frst) (cons frst rst)]
                               [else rst]))
          (lambda (frst rst) (cons frst rst)) empty lst))

;;Tests:
(check-expect (nfilter even? (list 1 (list 3 5) 7)) (list empty))
(check-expect (nfilter odd? (list 1 (list 3 5) 7)) (list 1 (list 3 5) 7))


;;Part C

;;(nmap f lst) produces the nested list that results from applying the consumed function
;; f to each element in lst
;;nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;;Examples:
(check-expect (nmap add1 (list 1 (list 2) 3)) (list 2 (list 3) 4))
(check-expect (nmap add1 empty) empty)

(define (nmap f lst)
  (nfoldr (lambda (frst rst) (cons (f frst) rst))
          (lambda (frst rst) (cons frst rst)) empty lst))

;;Tests:
(check-expect (nmap odd? (list 1 (list 2) 3)) (list true (list false) true))
(check-expect (nmap even? (list 1 (list 2) 3)) (list false (list true) false))
              

;;Part D

;;(nreverse lst) consumes lst and produces a new nested list with all of the elements
;; of lst mirrored
;;nreverse: Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (nreverse '(1 (2 3) () ((4)))) '(((4)) () (3 2) 1))
(check-expect (nreverse empty) empty)

(define (nreverse lst)
  (nfoldr (lambda (frst rst) (append rst (list frst)))
          (lambda (frst rst) (append rst (list frst))) empty lst))

;;Tests:
(check-expect (nreverse '((1 (2 3)) 4 (5 (6 7 8) 9))) '((9 (8 7 6) 5) 4 ((3 2) 1)))
(check-expect (nreverse (list (list empty))) (list (list empty)))


;;Part E

;;(nheight lst) consumes a lst and produces the height (maximum number of lists
;; that have been nested) of lst
;;nheight: Nested-Listof-X -> Nat
;;Examples:
(check-expect (nheight '()) 1)
(check-expect (nheight '(a b c)) 1)

(define (nheight lst)
  (nfoldr (lambda (frst rst)
            rst)
          (lambda (frst rst)
            (max (+ 1 frst) rst)) 1 lst))

;;Tests:
(check-expect (nheight '((1 a) (2 b) (3 c))) 2)
(check-expect (nheight '(1 (2 3) () ((4)))) 3)
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)


;;Part F

;;(prune lst) consumes lst and produces the nested list that results from removing all
;; empty lists and nested lists containing only empty lists from lst
;;prune: Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (prune '(1 (2 3 ()) ( (()) (4) () (())))) '(1 (2 3) ((4))))
(check-expect (prune (list (list empty))) empty)

(define (prune lst)
  (nfoldr (lambda (frst rst) (cons frst rst))
          (lambda (frst rst) (cond [(empty? frst) rst]
                                   [else (cons frst rst)])) empty lst))

;;Tests:
(check-expect (prune '(()((())()))) '())

