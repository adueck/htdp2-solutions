;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-037) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; returns a string like the one given with the
; first character removed
; given: "hello", expect: "ello"
; given: "world", expect: "orld"
(define (string-rest str)
  (substring str 1))
