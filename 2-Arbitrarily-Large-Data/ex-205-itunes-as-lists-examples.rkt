;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-205-itunes-as-lists-examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
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