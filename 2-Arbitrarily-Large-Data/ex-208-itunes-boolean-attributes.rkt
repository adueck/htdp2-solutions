;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-208-itunes-boolean-attributes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ITUNES-LOCATION "itunes.xml")

(define itunes-as-lists
  (read-itunes-as-lists ITUNES-LOCATION))

(define LA1
 (list
  (list "Track ID" 1)
  (list "Name" "Millie Monad")
  (list "Artist" "Function Frank")
  (list "Album" "Functional Fun")
  (list "Genre" "Techno")
  (list "Total Time" 227996)
  (list "Track Number" 1)
  (list "Track Count" 10)
  (list "Year" 2000)
  (list "Play Count" 3)
  (list "Favorite" #f)
  (list "Date Added" (create-date 2000 7 17 3 55 14))
  (list "Play Date" 3388484113)
  (list "Play Date UTC" (create-date 2011 5 17 17 35 13))))

(define LA2
 (list
  (list "Track ID" 1)
  (list "Name" "No side effects please")
  (list "Artist" "Function Frank")
  (list "Album" "Functional Fun")
  (list "Genre" "Techno")
  (list "Total Time" 227496)
  (list "Track Number" 2)
  (list "Track Count" 10)
  (list "Year" 2000)
  (list "Play Count" 5)
  (list "Favorite" #t)
  (list "Free" #t)
  (list "Date Added" (create-date 2001 8 17 3 55 14))
  (list "Play Date" 3388484113)
  (list "Play Date UTC" (create-date 2011 5 17 17 35 13))))

(define LA3
 (list
  (list "Track ID" 1)
  (list "Name" "Curry and chutney")
  (list "Artist" "Bill Jim")
  (list "Album" "Lambda Lounge")
  (list "Genre" "Techno")
  (list "Total Time" 222496)
  (list "Track Number" 1)
  (list "Track Count" 8)
  (list "Year" 2002)
  (list "Play Count" 5)
  (list "Date Added" (create-date 2003 3 5 8 32 11))
  (list "Play Date" 3388484113)
  (list "Play Date UTC" (create-date 2011 5 17 17 35 13))))

(define LL0 '())
(define LL1 (list LA1 LA2))
(define LL2 (list LA1 LA2 LA3))

; LLists -> List-of-strings
; produces a list of all the keys that refer to boolean
; attributes in a LLists
(check-expect (boolean-attributes LL0) '())
(check-expect (boolean-attributes LL2) (list "Favorite" "Free"))
(define (boolean-attributes ll)
  (create-set
   (cond
    [(empty? ll) '()]
    [else (append
           (get-boolean-keys (first ll))
           (boolean-attributes (rest ll)))])))

; LAssoc -> List-of-strings
; gets any boolean attributes in a LAssoc
(check-expect (get-boolean-keys '()) '())
(check-expect (get-boolean-keys LA2) (list "Favorite" "Free"))
(define (get-boolean-keys la)
  (cond
    [(empty? la) '()]
    [else (if (is-boolean-value? (first la))
              (cons (first (first la)) (get-boolean-keys (rest la)))
              (get-boolean-keys (rest la)))]))

; Assoc -> Boolean
; checks if an Assoc has a boolean value
(check-expect (is-boolean-value? (list "Favorite" #false)) #t)
(check-expect (is-boolean-value? (list "Name" "Bill")) #f)
(define (is-boolean-value? a)
  (boolean? (second a)))

; List-of-strings -> List-of-strings
(check-expect (create-set '()) '())
(check-expect (create-set (list "a" "a" "b" "b" "b" "c"))
              (list "a" "b" "c"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [else (if
           (member (first los) (rest los))
           (create-set (rest los))
           (cons (first los) (create-set (rest los))))]))

; > (boolean-attributes itunes-as-lists)
; (list "Disabled" "Compilation" "Purchased")
; Yes this makes sense, because these are all attributes
; which can be answered with a yes or no question
