;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 08, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insert-letters-hi (list
                           "ahi"
                           "bhi"
                           "chi"
                           "dhi"
                           "ehi"
                           "fhi"
                           "ghi"
                           "hhi"
                           "ihi"
                           "jhi"
                           "khi"
                           "lhi"
                           "mhi"
                           "nhi"
                           "ohi"
                           "phi"
                           "qhi"
                           "rhi"
                           "shi"
                           "thi"
                           "uhi"
                           "vhi"
                           "whi"
                           "xhi"
                           "yhi"
                           "zhi"
                           "hai"
                           "hbi"
                           "hci"
                           "hdi"
                           "hei"
                           "hfi"
                           "hgi"
                           "hhi"
                           "hii"
                           "hji"
                           "hki"
                           "hli"
                           "hmi"
                           "hni"
                           "hoi"
                           "hpi"
                           "hqi"
                           "hri"
                           "hsi"
                           "hti"
                           "hui"
                           "hvi"
                           "hwi"
                           "hxi"
                           "hyi"
                           "hzi"))

(define trail-hi (list
                  "hia"
                  "hib"
                  "hic"
                  "hid"
                  "hie"
                  "hif"
                  "hig"
                  "hih"
                  "hii"
                  "hij"
                  "hik"
                  "hil"
                  "him"
                  "hin"
                  "hio"
                  "hip"
                  "hiq"
                  "hir"
                  "his"
                  "hit"
                  "hiu"
                  "hiv"
                  "hiw"
                  "hix"
                  "hiy"
                  "hiz"))

(define replace-alpha-aaa (list
                           "aaa"
                           "aab"
                           "aac"
                           "aad"
                           "aae"
                           "aaf"
                           "aag"
                           "aah"
                           "aai"
                           "aaj"
                           "aak"
                           "aal"
                           "aam"
                           "aan"
                           "aao"
                           "aap"
                           "aaq"
                           "aar"
                           "aas"
                           "aat"
                           "aau"
                           "aav"
                           "aaw"
                           "aax"
                           "aay"
                           "aaz"))

(define replace-letters-hi (list
                            "ai"
                            "bi"
                            "ci"
                            "di"
                            "ei"
                            "fi"
                            "gi"
                            "hi"
                            "ii"
                            "ji"
                            "ki"
                            "li"
                            "mi"
                            "ni"
                            "oi"
                            "pi"
                            "qi"
                            "ri"
                            "si"
                            "ti"
                            "ui"
                            "vi"
                            "wi"
                            "xi"
                            "yi"
                            "zi"
                            "ha"
                            "hb"
                            "hc"
                            "hd"
                            "he"
                            "hf"
                            "hg"
                            "hh"
                            "hi"
                            "hj"
                            "hk"
                            "hl"
                            "hm"
                            "hn"
                            "ho"
                            "hp"
                            "hq"
                            "hr"
                            "hs"
                            "ht"
                            "hu"
                            "hv"
                            "hw"
                            "hx"
                            "hy"
                            "hz"))


;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(remove-dups slst) consumes slst and produces a new list with any duplicate Word in
;; slst removed
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;;Examples:
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups (list "Apple" "Butter")) (list "Apple" "Butter"))

(define (remove-dups slst)
  (foldr (lambda (frst rst)
           (cond [(empty? rst) (list frst)]
                 [(string=? frst (first rst)) rst]
                 [else (cons frst rst)])) empty slst))

;;Tests:
(check-expect (remove-dups (list "Apple" "Apple")) (list "Apple"))
(check-expect (remove-dups (list "apple" "apple" "apples" "banana" "cherry" "cherry"))
              (list "apple" "apples" "banana" "cherry"))


;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(ifoldr combine base lst) consumes a combine, base, and lst and produces the result
;; of applying the combine function to the first and the rest of lst if lst is not
;; empty and produces base if lst is empty
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;;Examples:
(check-expect (ifoldr (lambda (i x y) (* i x)) 1 (list 1 2 3)) 0)
(check-expect (ifoldr (lambda (i x y) (* i x)) 0 empty) 0)

(define (ifoldr combine base lst)
  (local
    [(define (ifoldr-index i combine base lst)
       (cond
         [(empty? lst) base]
         [else (combine i (first lst)
                        (ifoldr-index (add1 i) combine base (rest lst)))]))]
    (ifoldr-index 0 combine base lst)))

;;Tests:
(check-expect (ifoldr (lambda (i x y) (+ y (* i x))) 1 (list 1 2 3)) 9)
(check-expect (ifoldr (lambda (i x y) (* i x y)) 0 (list 4 5 4 5 5)) 0)


;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))


;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))


;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(insert-char-before-i i char loc) produces a string built from a list of characters 
;; with char inserted before the element of loc at index position i
;;insert-char-before-i: Nat Char (listof Char) -> Word
;;requires: i is a valid index position for loc (i+1 <= length of loc)
;;          Char is a lowercase letter and loc contains only lowercase letters 
;;Examples:
(check-expect (insert-char-before-i 0 #\c (list #\a #\b)) "cab")
(check-expect (insert-char-before-i 1 #\c (list #\a #\b)) "acb")

(define (insert-char-before-i i char loc)
  (list->string (ifoldr (lambda (k frst rst)
                          (cond
                            [(= i k) (append (cons char (list frst)) rst)]
                            [else (cons frst rst)])) empty loc)))


;;(insert-alpha-before-i i loc) produces a list of strings built from lists of characters
;; with every letter in the alphabet inserted before the element of loc at index
;; position i
;;insert-alpha-before-i: Nat (listof Char) -> (listof Word)
;;requires: loc contains only lowercase letters as characters
;;Examples:
(check-expect (insert-alpha-before-i 0 (list #\h #\i))
              (list
               "ahi" "bhi" "chi" "dhi" "ehi" "fhi" "ghi" "hhi" "ihi" "jhi" "khi"
               "lhi" "mhi" "nhi" "ohi" "phi"  "qhi" "rhi" "shi" "thi" "uhi"
               "vhi" "whi"  "xhi"  "yhi"  "zhi"))

(define (insert-alpha-before-i i loc)
  (foldr (lambda (frst rst) (cons (insert-char-before-i i frst loc) rst)) empty letters))


;;(insert-letters s) produces a list of words with each letter of the alphabet inserted
;; before each letter of s
;; insert-letters: Word -> (listof Word)
;;Examples:
(check-expect (insert-letters "hi") insert-letters-hi)

(define (insert-letters s)
  (local
    [(define loc (string->list s))]
    (ifoldr (lambda (i frst rst) (append (insert-alpha-before-i i loc) rst)) empty loc)))


;;(trailing-letters s) consumes s and produces a list of words with each letter of
;; the alphabet inserted after s
;; trailing-letters: Word -> (listof Word)
;;Examples:
(check-expect (trailing-letters "hi") trail-hi)

(define (trailing-letters s)
  (local
    [(define loc (string->list s))] 
    (foldr (lambda (frst rst)
             (cons (list->string (append loc (list frst))) rst)) empty letters)))


;;(replace-char-i i char loc) produces a string built from a list of characters where
;; char replaces the character at index position i of loc
;;replace-char-i: Nat Char (listof Char) -> Word
;;requires: Char is a lowercase letter and loc contains only lowercase letters
;;Examples:
(check-expect (replace-char-i 1 #\c (list #\a #\a #\a)) "aca")

(define (replace-char-i i char loc)
  (list->string (ifoldr (lambda (k frst rst) (cond
                                               [(= i k) (cons char rst)]
                                               [else (cons frst rst)])) empty loc)))


;;(replace-alpha-i i loc) produces a list of strings built from lists of characters with
;; every letter in the alphabet replacing the character at index position i of loc
;;replace-alpha-i: Nat (listof Char) -> (listof Word)
;;requires: Char is a lowercase letter and loc contains only lowercase letters
;;Examples:
(check-expect (replace-alpha-i 2 (list #\a #\a #\a)) replace-alpha-aaa)

(define (replace-alpha-i i loc)
  (foldr (lambda (frst rst) (cons (replace-char-i i frst loc)
                                  rst)) empty letters))


;;(replace-letters s) produces a list of words with every letter of the alphabet
;; replacing every character in s
;;replace-letters: Word -> (listof Word)
;;Examples:
(check-expect (replace-letters "hi") replace-letters-hi)

(define (replace-letters s)
  (local
    [(define loc (string->list s))]
    (ifoldr (lambda (i frst rst) (append (replace-alpha-i i loc)
                                         rst)) empty loc)))


;;(loc-at-i i lst) produces the character at index position i of lst
;;loc-at-i: Nat (listof Char) -> Char
;;Examples:
(check-expect (loc-at-i 0 (list #\a #\b)) #\a)

(define (loc-at-i i lst)
  (ifoldr (lambda (k frst rst) (cond
                                 [(= i k) frst]
                                 [else rst])) empty lst))


;;(swap-at-i i loc) produces a string built from a list of characters where the
;; characters at index position i and i+1 of loc are swapped
;;swap-at-i: Nat (listof Char) -> Word
;;requires: loc contains only lowercase letters
;;Examples:
(check-expect (swap-at-i 1 (list #\a #\b #\c)) "acb")

(define (swap-at-i i loc)
  (list->string (ifoldr (lambda (k frst rst)
                          (cond
                            [(= i k) (append (cons (loc-at-i (add1 k) loc)
                                                   (cons (loc-at-i k loc) rst)))]
                            [(= (add1 i) k) rst]
                            [else (cons frst rst)])) empty loc)))


;;(swap-letters s) produces a list of words with every adjacent pair of letters in
;; s swapped 
;; swap-letters: Word -> (listof Word)
;;Examples:
(check-expect (swap-letters "maple") (list "ample" "mpale" "malpe" "mapel"))
(check-expect (swap-letters "hi") (list "ih"))

(define (swap-letters s)
  (local
    [(define loc (string->list s))]
    (build-list (sub1 (length loc)) (lambda (i) (swap-at-i i loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You are not required to modify the definition of suggest,
;; but you may if you wish

;;(suggest s valid?) consumes a Word (s) and a predicate function for determining
;; if s is spelled correctly and produces a list of correctly spelled words that
;; are "close" to s
;; suggest: Word (Word -> Bool) -> (listof Word)
;;Examples:
(check-expect (suggest "right" valid?) (list "bright" "fight" "rights"))

(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))


(define (valid? s)
  (member? s '("rights" "right" "fight" "aardvark" "fhqwhgads" "bright")))

;;Tests:
(check-expect (suggest "rights" valid?) (list "right"))
(check-expect (suggest "fight" valid?) (list "right"))

