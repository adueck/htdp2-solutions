;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-275-dictionaries-refactor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define LOCATION "words")

; A Dictionary is a List-of-strings sorted in alphabetical order

; A Sub-Dictionary is a Dictionary of words starting with the same letter
; sorted in alphabetical order

; A List-of-Sub-Dictionarys is one of the following
; - '()
; - (cons Sub-Dictionary List-of-sub-dictionaries)
; constraint: the list of Sub-Dictionarys is in alphabetical order 

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

(define TESTING-DICT
  (list "able" "apple" "car"))
(define DICTIONARY
  (read-lines LOCATION))

; Dictionary -> List-of-Sub-Dictionarys
; Breaks up one dictionary into a list of sub-dictionaries, one for each letter
(check-expect (words-by-first-letter TESTING-DICT)
              (list
               (list "able" "apple")
               (list "car")))
(define (words-by-first-letter d)
  (foldl add-word-to-sub-dicts '() d))

; Dictionary -> Letter-Count
; Shows the letter which begins the most words
; in a given dictionary along with its count
(check-expect (most-frequent TESTING-DICT)
              (make-lc "a" 2))
(define (most-frequent d)
  (first (sort-lolc> (count-by-letter d LETTERS))))

; String List-of-Sub-Dictionaries -> List-of-Sub-Dictionaries
; Adds a word to the appropriate dictionary in a list of dictionaries by starting letter
(check-expect (add-word-to-sub-dicts "fbb"
                                    (list
                                     (list "faa" "fcc")
                                     (list "game" "grow")))
              (list
               (list "faa" "fbb" "fcc")
               (list "game" "grow")))
(check-expect (add-word-to-sub-dicts "green"
                                    (list
                                     (list "fred" "friend")
                                     (list "late" "loose")))
              (list
                (list "fred" "friend")
                (list "green")
                (list "late" "loose")))
(define (add-word-to-sub-dicts w d)
  ; make sure we always have a sorted list of sub-dictionaries
  (sort-dicts<
   (cond
    ; no matching existing dictionary create a new sub-dictionary
    [(empty? d) (list (list w))]
    ; try to add the word to an existing/matching sub-dictionary
    [else (if (word-belongs? w (first d))
              (cons
               (insert-word w (first d)) (rest d))
              (cons (first d) 
                    (add-word-to-sub-dicts w (rest d))))])))

; String -> Sub-Dictionary
; checks to see if a word belongs in a dictionary of words starting with one letter
(check-expect (word-belongs? "foo" (list "bar" "bat")) #false)
(check-expect (word-belongs? "foo" (list "food" "fun")) #true)
(define (word-belongs? w d)
  (string=?
   (substring w 0 1)
   (substring (first d) 0 1)))

; List-of-Sub-Dictionarys -> List-of-Sub-Dictionarys
; sorts a list of letter-based dictionaries in alphabetical order
(check-expect
 (sort-dicts< (list
              (list "car")
              (list "able" "apple")))
             (list
              (list "able" "apple")
              (list "car")))
(define (sort-dicts< lod)
  (local (; Sub-Dictionary Sub-Dictionary -> Boolean
          (define (dict-is-earlier? d1 d2)
            (local (; Sub-Dictionary -> 1String
                    (define (getFirstLetter d)
                      (substring (first d) 0 1)))
              (string<? (getFirstLetter d1) (getFirstLetter d2)))))
    (sort lod dict-is-earlier?)))

; Sub-Dictionary -> 1String
; gets the letter of a a letter-based dictionary
(check-expect (letter-of-dict (list "far" "foo")) "f")
(define (letter-of-dict d)
  (substring (first d) 0 1))

; String -> List-of-strings
; inserts a word in it's proper place in an alpabetically sorted list of strings
(check-expect (insert-word "bear" (list "apple" "car"))
              (list "apple" "bear" "car"))
(check-expect (insert-word "zebra" (list "a" "b"))
              (list "a" "b" "zebra"))
(check-expect (insert-word "first" '()) (list "first"))
(define (insert-word w los)
  (cond
    [(empty? los) (cons w '())]
    [else
     (if (string<=? w (first los))
         (cons w los)
         (cons (first los) (insert-word w (rest los))))]))

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

; List-of-sub-dictionaries -> Sub-dictionary
; find the longest list-of-sub-dictionaries
(check-expect (longest-sub-dict
               (list
                (list "a" "apple")
                (list "b" "ba" "bc")
                (list "cool")))
              (list "b" "ba" "bc"))
(define (longest-sub-dict losd)
  (cond
    [(empty? (rest losd)) (rest losd)]
    [else (if
           (>= (length (first losd))
               (length
                (longest-sub-dict (rest losd))))
           (first losd)
           (longest-sub-dict (rest losd)))]))

; Sub-Dictionary -> Letter-Count
; gives the letter and the number of entries of a given Sub-Dictionary
(check-expect (sub-dict-info (list "a" "apple" "art"))
              (make-lc "a" 3))
(define (sub-dict-info sd)
  (make-lc
   (letter-of-dict sd)
   (length sd)))

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
  (local (; Letter-count Letter-count -> Boolean
          (define (letter-count>? lc1 lc2)
            (> (lc-number lc1) (lc-number lc2))))
  (sort lolc letter-count>?)))


