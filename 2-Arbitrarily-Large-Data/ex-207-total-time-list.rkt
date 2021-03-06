;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-207-total-time-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ITUNES-LOCATION "itunes.xml")

(define itunes-as-lists
  (read-itunes-as-lists ITUNES-LOCATION))

(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

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

; LLists -> Number
; Gives the total amount of play time for an LLists
(check-expect (total-time/lists LL0) 0)
(check-expect (total-time/lists LL1) 455492) 
(define (total-time/lists ll)
  (cond
    [(empty? ll) 0]
    [else (+
           (get-time/list (first ll))
           (total-time/lists (rest ll)))]))

; LTracks -> Number
; Gives the total amount of playtime for a list of tracks
(check-expect (total-time '()) 0)
;(check-expect (total-time LT) 700)
(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [else (+
           (track-time (first lt))
           (total-time (rest lt)))]))

; LAssoc -> Number
; gets the time of a track in list representation
(check-expect (get-time/list LA2) 227496) 
(define (get-time/list la)
  (second (assoc "Total Time" la)))

; String LAssoc Any -> Association
(define not-found (list "none found" 0))
(check-expect (find-association "Name" '() not-found) not-found)
(check-expect (find-association "Name" LA1 not-found)
              (list "Name" "Millie Monad"))
(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else (if (key-matches? key (first la))
              (first la)
              (find-association key (rest la) default))]))

; String LAssoc -> Boolean
; checks if a given key matches an LAssoc
(check-expect (key-matches? "foo" (list "foo" "bar")) #t)
(check-expect (key-matches? "bar" (list "foo" "bar")) #f)
(define (key-matches? key la)
  (string=? key (first la)))

;> (total-time itunes-tracks)
;467897977
;> (total-time itunes-as-lists)
;494030470
; The second one could be bigger because it's more general
; and covers more media types??
