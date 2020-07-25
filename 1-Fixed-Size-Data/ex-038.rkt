;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-038) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; returns a string like the one given but with the last
; character removed
; given: "foo", expect: "fo"
; given: "bar", expect: "ba"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))
