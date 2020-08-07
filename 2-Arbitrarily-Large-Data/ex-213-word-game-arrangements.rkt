;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-213-word-game-arrangements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define LOCATION "words")

; A Dictionary is a List-of-strings.
(define DICTIONARY (read-lines LOCATION))

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define WORD0 '())
(define WORD1 (list "d" "o" "o" "r"))
(define WORD2 (list "b" "a" "t"))
 
; A List-of-words is on of:
; - '() or
; - (cons Word List-of-words)
(define LOW1 '())
(define LOW2 (list WORD1 WORD2))

; String -> List-of-strings
; finds all words that the letters of some given word spell
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 
; Word -> List-of-words
; finds all rearrangements of word
(check-expect (arrangements '()) (list '()))
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e")
                    (list "e" "d")))
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
           (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
; Inserts s in every position in every word in the list of words
(check-expect (insert-everywhere/in-all-words "a" '())
              '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "d")))
              (list (list "a" "d")
                    (list "d" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "d" "e")
                                                        (list "e" "d")))
              (list (list "a" "d" "e")
                    (list "d" "a" "e")
                    (list "d" "e" "a")
                    (list "a" "e" "d")
                    (list "e" "a" "d")
                    (list "e" "d" "a")))
(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) '()]
    [else (append
           (insert-everywhere/in-one-word s (first low))
           (insert-everywhere/in-all-words s (rest low)))]))

; 1String Word -> List-of-words
; Gives a List-of-words with s inserted into every possible position in a Word
(check-expect (insert-everywhere/in-one-word "a" '())
              (list (list "a")))
(check-expect (insert-everywhere/in-one-word "a" (list "e"))
              (list (list "a" "e")
                    (list "e" "a")))
(check-expect (insert-everywhere/in-one-word "a" (list "d" "e"))
              (list (list "a" "d" "e")
                    (list "d" "a" "e")
                    (list "d" "e" "a")))
(define (insert-everywhere/in-one-word s w)
  (cond
    [(empty? w) (list (list s))]
    [else (cons
           ; add one more word on the top of the list with the s
           ; in front of the full word
           (cons s w)
           ; stick the current 1String in front of each of the existing
           ; words
           (append-to-each-word
            (first w) (insert-everywhere/in-one-word s (rest w))))]))

; 1String List-of-words -> List-of-words
; adds s on the front of each word in a list
(check-expect (append-to-each-word "a" '()) '())
(check-expect (append-to-each-word "a" (list (list "r" "e")))
              (list (list "a" "r" "e")))
(check-expect (append-to-each-word "a" (list (list "r" "e")
                                        (list "t" "e")))
              (list (list "a" "r" "e")
                    (list "a" "t" "e")))         
(define (append-to-each-word s low)
  (cond
    [(empty? low) '()]
    [else (cons
           (cons s (first low))
           (append-to-each-word s (rest low)))]))

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "") '())
(check-expect (string->word "cat")
              (list "c" "a" "t"))
(define (string->word s)
  (explode s))
 
; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "c" "a" "t"))
              "cat")
(define (word->string w)
  (implode w))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
  
; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list
                              (list "c" "a" "t")
                              (list "b" "e" "a" "r")))
              (list "cat" "bear"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons
           (word->string (first low))
           (words->strings (rest low)))]))
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "cat" "bear" "cgxbljrcjkm"))
              (list "cat" "bear"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (exists-in-dict? (first los) DICTIONARY)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))

; String Dictionary -> Boolean
; tells if an existing word (as a string) exists in the Dictionary
(check-expect (exists-in-dict? "apple" (list "apple" "car"))
              #true)
(check-expect (exists-in-dict? "bkorceauh" (list "bear" "table"))
              #false)
(define (exists-in-dict? s dict)
  (cond
    [(empty? dict) #f]
    [else (or
           (string=? (first dict) s)
           (exists-in-dict? s (rest dict)))]))