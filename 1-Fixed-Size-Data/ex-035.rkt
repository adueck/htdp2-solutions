;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-035) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> 1String
; returns the last character of a string
; given: "hello", expect: "o"
; given: "world", expect: "d"
(define (string-last str)
  (string-ith str (- (string-length str) 1)))


