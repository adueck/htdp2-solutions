;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-289) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)

(check-expect (find-name "Joe" '()) #f)
(check-expect (find-name "Rob" (list "Robert" "Joe")) #t)
; [List-of String] -> Boolean
(define (find-name n lon)
  (ormap
   (lambda (x) (string-prefix? (string-downcase x) (string-downcase n)))
   lon))

(check-expect (all-start-with-a? (list "Alfred" "Abe" "Amy")) #t)
(check-expect (all-start-with-a? (list "Bill" "Tom" "Arnie")) #f)
; [List-of String] -> Boolean
(define (all-start-with-a? lon)
  (andmap (lambda (x) (string-prefix? (string-downcase x) "a")) lon))

; ormap would be more efficient for defining a function that ensures that
; no name on some list exceeds some given width, because you could set it
; to check if at least one element is over the given width, and you wouldn't
; have to go through and check every item after finding 1 item that exceeds the length

