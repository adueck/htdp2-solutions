;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-259-word-games-refactor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
                 (local
                   (; List-of-strings -> Boolean
                    (define (all-words-from-rat? w)
                      (and
                       (member? "rat" w)
                       (member? "art" w)
                       (member? "tar" w))))
                 all-words-from-rat?))
(define (alternative-words s)
  (local
    (; 1. Find all the combination of rearranged letters
     (define possible-words
       (local
         (; String -> Word
          ; converts s to the chosen word representation
          (define (string->word s)
            (explode s))
          ; Word -> List-of-words
          ; finds all rearrangements of word
          (define (arrangements w)
            (cond
              [(empty? w) (list '())]
              [else (local
                      (; 1String List-of-words -> List-of-words
                       ; Inserts s in every position in every word in the list of words
                       (define (insert-everywhere/in-all-words s low)
                         (cond
                           [(empty? low) '()]
                           [else (append
                                  (insert-everywhere/in-one-word s (first low))
                                  (insert-everywhere/in-all-words s (rest low)))]))
                       ; 1String List-of-words -> List-of-words
                       ; adds s on the front of each word in a list         
                       (define (append-to-each-word s low)
                         (cond
                           [(empty? low) '()]
                           [else (cons
                                  (cons s (first low))
                                  (append-to-each-word s (rest low)))]))
                       ; 1String Word -> List-of-words
                       ; Gives a List-of-words with s inserted into every possible position in a Word
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
                                   (first w) (insert-everywhere/in-one-word s (rest w))))])))
           (insert-everywhere/in-all-words
            (first w)
            (arrangements (rest w))))])))
         (arrangements (string->word s))))
     ; 2. Make these lists of combination into entry strings
     (define possible-entries
       (local
         (; List-of-words -> List-of-strings
          ; turns all Words in low into Strings
          (define (words->strings low)
            (cond
              [(empty? low) '()]
              [else (local
                     (; Word -> String
                      ; converts w to a string
                      (define (word->string w)
                        (implode w)))
                     (cons
                      (word->string (first low))
                      (words->strings (rest low))))])))
         (words->strings possible-words)))
     ; 3. See which of these are actually entries in the dictionary
     (define answer
       (local
         (; List-of-strings -> List-of-strings
          ; picks out all those Strings that occur in the dictionary
          (define (in-dictionary los)
            (cond
              [(empty? los) '()]
              [else (local
                      (; String Dictionary -> Boolean
                       ; tells if an existing word (as a string) exists in the Dictionary
                       (define (exists-in-dict? s dict)
                         (cond
                           [(empty? dict) #f]
                           [else (or
                                  (string=? (first dict) s)
                                  (exists-in-dict? s (rest dict)))])))
                      (if (exists-in-dict? (first los) DICTIONARY)
                          (cons (first los) (in-dictionary (rest los)))
                          (in-dictionary (rest los))))])))
         (in-dictionary possible-entries))))
    answer))