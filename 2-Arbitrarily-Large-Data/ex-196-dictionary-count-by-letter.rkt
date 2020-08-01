;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-196-dictionary-count-by-letter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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



