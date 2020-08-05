;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-200-itunes-total-time) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define LT
  (cons TRACK1 (cons TRACK2 '())))

; LTracks -> Number
; Gives the total amount of playtime for a list of tracks
(check-expect (total-time '()) 0)
(check-expect (total-time LT) 700)
(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [else (+
           (track-time (first lt))
           (total-time (rest lt)))]))