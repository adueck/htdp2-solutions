;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-204-itunes-select-albums) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
              DATE2))
(define TRACK3
  (create-track "Morty the Monad" "Frank Smith" "Lambda Lounge" 500 2
              DATE2
              3
              DATE4))
(define LT
  (cons TRACK1 (cons TRACK2 '())))
(define LT2
  (cons TRACK1 (cons TRACK2 (cons TRACK3 '()))))

; LTracks -> LTracks
; Produces a list of a track from each album
(check-expect (select-albums '()) '())
(check-expect (select-albums LT2)
              (list TRACK1 TRACK2))
(define (select-albums lt)
  (get-sample-tracks (select-all-album-titles/unique lt) lt))

; List-of-strings LTracks -> LTracks
; makes a list of one track off of each album in a list
(check-expect (get-sample-tracks '() '()) '())
(check-expect (get-sample-tracks (list "Lambda Lounge") LT2)
              (list TRACK2))
(define (get-sample-tracks loa lt)
  (cond
    [(empty? loa) '()]
    [else (cons
           (get-sample-track (first loa) lt)
           (get-sample-tracks (rest loa) lt))]))

; String LTracks -> Track
; gets one track from a given album
(define (get-sample-track a lt)
  (first (select-album a lt)))

; String Date LTracks -> LTracks
; Selects all tracks from a give album that have been
; played after a given date
(check-expect (select-album-date "Doesn't Exist" DATE1 LT2) '())
(check-expect (select-album-date "Lambda Lounge" DATE1 LT2)
              (list TRACK2 TRACK3))
(check-expect (select-album-date "Lambda Lounge" DATE3 LT2)
              (list TRACK3))
(define (select-album-date a d lt)
  (cond
    [(empty? lt) '()]
    [else (if
           (and
            (before? d (track-played (first lt)))
            (string=? a (track-album (first lt))))
           (cons (first lt)
                 (select-album-date a d (rest lt)))
           (select-album-date a d (rest lt)))]))

; Date Date -> Boolean
; Checks if the first date is more recent than the second
(check-expect (before? DATE1 DATE1) #f)
(check-expect (before? DATE1 DATE2) #t)
(check-expect (before? DATE2 DATE1) #f)
; solution thanks to https://github.com/eareese/htdp-exercises/
(define (before? d1 d2)
  (cond
    [(< (date-year d1) (date-year d2)) #t]
    [(> (date-year d1) (date-year d2)) #f]
    [(< (date-month d1) (date-month d2)) #t]
    [(> (date-month d1) (date-month d2)) #f]
    [(< (date-day d1) (date-day d2)) #t]
    [(> (date-day d1) (date-day d1)) #f]
    [(< (date-hour d1) (date-hour d2)) #t]
    [(> (date-hour d1) (date-hour d1)) #f]
    [(< (date-minute d1) (date-minute d2)) #t]
    [(> (date-minute d1) (date-minute d1)) #f]
    [(< (date-second d1) (date-second d2)) #t]
    [else #f]))
              
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