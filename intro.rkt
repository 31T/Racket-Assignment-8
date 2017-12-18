;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 08, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Part A
;;(keep-ints loa) consumes a (listof Any) loa and produces a (listof Int) with all of
;; the integer elements in loa
;;keep-ints: (listof Any) -> (listof Int)
;;Examples:
(check-expect (keep-ints (list 1 2 3 'Hi)) (list 1 2 3))
(check-expect (keep-ints empty) empty)

(define (keep-ints loa)
  (filter integer? loa))

;;Tests:
(check-expect (keep-ints (list 'Hi 'Hello 'Hillo)) empty)
(check-expect (keep-ints (list 1 2)) (list 1 2))


;;Part B

;;(contains? elem loa) consumed an elem and a loa and produces true if the consumed elem
;; is in loa and false otherwise
;;contains?: Any (listof Any) -> Bool
;;Examples:
(check-expect (contains? 2 (list 2)) true)
(check-expect (contains? 2 empty) false)

(define (contains? elem loa)
  (cons? (filter (lambda (frst) (equal? elem frst)) loa)))

;;Tests:
(check-expect (contains? empty empty) false)
(check-expect (contains? 'Hi (list 'Hill)) false)


;;Part C
;;(lookup-al key al) produces the value corresponding to key or false if key is not
;; present
;;lookup-al: Num AL -> (anyof Str false)
;;Examples:
(check-expect (lookup-al 1 (list (list 1 "h") (list 2 "b"))) "h")
(check-expect (lookup-al 1 empty) false)

(define (lookup-al key al)
  (foldr (lambda (frst result)
           (cond [(= key (first frst)) (second frst)]
                 [else result])) false al))

;;Tests:
(check-expect (lookup-al 1 (list (list 2 "h"))) false)
(check-expect (lookup-al 2.5 (list (list 2.5 "BH"))) "BH")


;;Part D

;;(extract-keys al) consumes an al and produces a list of all the keys in the consumed
;; AL (al)
;;extract-keys: AL -> (listof Num)
;;Examples:
(check-expect (extract-keys (list (list 1 "h") (list 2 "c"))) (list 1 2))
(check-expect (extract-keys empty) empty)

(define (extract-keys al)
  (foldr (lambda (frst result) (cons (first frst) result)) empty al))

;;Tests:
(check-expect (extract-keys (list (list 1 "BH"))) (list 1))
(check-expect (extract-keys (list (list 1.5 "B"))) (list 1.5))


;;Part E

;;(sum-positive loi) consumes a (listof Int) loi and produces the sum of all the
;; positive integers in loi
;;sum-positive: (listof Int) -> Nat
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive (list 1)) 1)

(define (sum-positive loi)
  (foldr (lambda (frst result)
           (cond [(> frst 0) (+ frst result)]
                 [else result])) 0 loi))

;;Tests:
(check-expect (sum-positive (list -1 -2)) 0)
(check-expect (sum-positive (list 2 3 4)) 9)


;;Part F

;;(countup-to n b) consumed n and b and produces a list of integers increasing by 1,
;; starting from n and ending at b
;;countup-to: Int Int -> (listof Int)
;;requires: n <= b
;;Examples:
(check-expect (countup-to 6 6) (list 6))
(check-expect (countup-to 4 5) (list 4 5))

(define (countup-to n b)
  (build-list (add1 (- b n)) (lambda (x) (+ x n))))

;;Tests:
(check-expect (countup-to 0 2) (list 0 1 2))
(check-expect (countup-to -5 -4) (list -5 -4))


;;Part G

;;(shout los) consumes a los and produces a new list of strings with all of the 
;; strings in the consumed (listof Str) los, but in uppercase
;;shout: (listof Str) -> (listof Str)
;;Examples:
(check-expect (shout empty) empty)
(check-expect (shout (list "hi")) (list "HI"))

(define (shout los)
  (map (lambda (char) (list->string (foldr
                                     (lambda (frst result)
                                       (cons (char-upcase frst) result))
                                     empty (string->list char))))
       los))

;;Tests:
(check-expect (shout (list "")) (list ""))
(check-expect (shout (list "Hello" "GoodBye")) (list "HELLO" "GOODBYE"))


;;Part H

;;(make-validator loa) consumes a (listof X) loa and produces a predicate function
;; which consumes a single item of type X and produces a boolean that determines
;; if the item appears in the list that was consumed by our function
;;make-validator: (listof Any) -> (Any -> Bool)
;;Examples:
(check-expect (primary-colour? 'red) true)
(check-expect (primary-colour? 'redd) false)

(define (make-validator loa)
  (local
    [(define (pred? item)
       (foldr (lambda (frst rst) (cond [(equal? frst item) true]
                                       [else rst])) false loa))]
    pred?))

(define primary-colour? (make-validator '(red blue green)))

;;Tests:
(check-expect (primary-colour? 'blue) true)
(check-expect (primary-colour? 'green) true)

