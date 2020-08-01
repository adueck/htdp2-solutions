;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-197-dictionary-most-frequent) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define LOCATION "words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; An Alphabet is a List-of-letters

; A Letter-Counts is a structure
(define-struct lc [letter number])
; (make-ls [1String Number)
; interpretation: for any given letter, the number of words
; starting with that letter

; A List-of-letter-counts is one of the following:
; - '()
; - (cons Letter-Count '())

(define DICTIONARY
  (read-lines LOCATION))
(define TESTING-DICT
  (list "able" "apple" "car"))

; Dictionary Alphabet -> List-of-letter-counts
; Produces the counts for words starting with each letter in an alphabet
(check-expect (count-by-letter '() '()) '())
(check-expect (count-by-letter TESTING-DICT (explode "abc"))
              (list (make-lc "a" 2)
                    (make-lc "b" 0)
                    (make-lc "c" 1)))
(define (count-by-letter d a)
  (cond
    [(empty? a) '()]
    [else (cons
           (make-lc
            (first a)
            (starts-with# d (first a)))
           (count-by-letter d (rest a)))]))

; Dictionary 1String -> Number
; gives the amount of words that start with 1String
(check-expect (starts-with# TESTING-DICT "a") 2)
(check-expect (starts-with# TESTING-DICT "c") 1)
(check-expect (starts-with# TESTING-DICT "f") 0)
(define (starts-with# d l)
  (cond
    [(empty? d) 0]
    [else (+
           (if (begins-with? (first d) l) 1 0)
           (starts-with# (rest d) l))]))

; String 1String -> Boolean
; Determines if a string begins with a certain letter
(check-expect (begins-with? "apple" "a") #true)
(check-expect (begins-with? "pear" "f") #false)
(define (begins-with? s l)
  (string=? (first (explode s)) l))

; Dictionary -> Letter-Count
; Shows the letter which begins the most words
; in a given dictionary along with its count
(check-expect (most-frequent TESTING-DICT)
              (make-lc "a" 2))
(check-expect (most-frequent2 TESTING-DICT)
              (make-lc "a" 2))
(define (most-frequent d)
  (highest-count (count-by-letter d LETTERS)))
(define (most-frequent2 d)
  (first (sort-lolc> (count-by-letter d LETTERS))))

; List-of-letter-counts -> Letter-Count
; returns the Letter-Count for the letter with the
; highest frequency
(check-expect (highest-count (count-by-letter TESTING-DICT LETTERS))
              (make-lc "a" 2))
(define (highest-count lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (higher-count
            (first lolc)
            (highest-count (rest lolc)))]))

; Letter-Count Letter-Count -> Letter-Count
; Returns the greater of two letter counts,
; the first one winning in a draw
(check-expect (higher-count
               (make-lc "a" 4)
               (make-lc "b" 7))
              (make-lc "b" 7))
(check-expect (higher-count
               (make-lc "m" 5)
               (make-lc "n" 5))
              (make-lc "m" 5))
(define (higher-count c1 c2)
  (if (> (lc-number c2) (lc-number c1))
      c2 c1))

; Letter-Count Letter-Count -> Boolean
; Checks if the first letter count is higher than the second
(check-expect (higher-count? (make-lc "a" 4)
                             (make-lc "c" 6))
              #false)
(check-expect (higher-count? (make-lc "a" 6)
                             (make-lc "c" 2))
              #true)
(define (higher-count? c1 c2)
  (> (lc-number c1) (lc-number c2)))

; List-of-letter-counts -> List-of-letter-counts
; Sorts a list of letter counts by frequency
(check-expect (sort-lolc> '()) '())
(check-expect (sort-lolc> (list
                           (make-lc "a" 10)
                           (make-lc "b" 3)
                           (make-lc "c" 23)))
                          (list
                           (make-lc "c" 23)
                           (make-lc "a" 10)
                           (make-lc "b" 3)))
(define (sort-lolc> lolc)
  (cond
    [(empty? lolc) '()]
    [else (insert-lc
           (first lolc)
           (sort-lolc> (rest lolc)))]))

; Letter-count -> List-of-letter-counts
; inserts a Letter-count in its proper place in
; a sorted list of letter counts
(check-expect (insert-lc (make-lc "a" 4) '())
              (list (make-lc "a" 4)))
(check-expect (insert-lc (make-lc "a" 10)
                         (list
                          (make-lc "c" 23)
                          (make-lc "b" 3)))
              (list
               (make-lc "c" 23)
               (make-lc "a" 10)
               (make-lc "b" 3)))
(define (insert-lc lc lolc)
 (cond
   [(empty? lolc) (cons lc '())]
   [else (if (higher-count?
              lc
              (first lolc))
             (cons lc lolc)
             (cons (first lolc) (insert-lc lc (rest lolc))))]))             
  
; the most frequently used letter for starting
; words on my computer's dictionary is "s", which
; is used 10019 times

; I preferred to design the function that picks
; the highest count rather than sorting the whole
; list because it seemed like the simplest and
; most straightforward solution. Both solutions
; are included though.
