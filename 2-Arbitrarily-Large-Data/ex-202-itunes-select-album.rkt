;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-202-itunes-select-album) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define DATE1
  (create-date 2000 1 1 6 30 45))
(define DATE2
  (create-date 2010 2 2 12 10 15))
(define DATE3
  (create-date 2015 2 1 12 10 15))
(define DATE4
  (create-date 2020 6 2 12 10 15))

(define TRACK1
  (create-track "Coding Blues" "Bill Jones" "Songs on Software" 200 1
              DATE1
              5
              DATE2))
(define TRACK2
  (create-track "Functional Fun" "Frank Smith" "Lambda Lounge" 500 2
              DATE2
              3
              DATE4))
(define TRACK3
  (create-track "Morty the Monad" "Frank Smith" "Lambda Lounge" 500 2
              DATE2
              3
              DATE4))
(define LT
  (cons TRACK1 (cons TRACK2 '())))
(define LT2
  (cons TRACK1 (cons TRACK2 (cons TRACK3 '()))))

; String LTracks -> LTracks
; Selects all tracks from a given album
(check-expect (select-album "Doesn't Exist" LT2) '())
(check-expect (select-album "Lambda Lounge" LT2)
              (list TRACK2 TRACK3))
(define (select-album a lt)
  (cond
    [(empty? lt) '()]
    [else (if
           (string=? a (track-album (first lt)))
           (cons (first lt) (select-album a (rest lt)))
           (select-album a (rest lt)))]))

; LTracks -> List-of-strings
; selects all the album titles from a LTracks
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles LT)
              (list "Songs on Software" "Lambda Lounge"))
(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [else (cons
           (track-album (first lt))
           (select-all-album-titles (rest lt)))]))

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

; LTracks -> List-of-strings
(check-expect (select-all-album-titles/unique '()) '())
(check-expect (select-all-album-titles/unique (cons TRACK3 LT))
              (list "Songs on Software" "Lambda Lounge"))
(define (select-all-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))