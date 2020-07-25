;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-066) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; Constructor
; String String Number -> movie
; (make-movie ...)
(make-movie "Pele" "Jeff Zimbalist" 2016)

; Selectors
; (movie-title ...)
; (movie-producer ...)
; (movie-year ...)
; Predicate
; (movie? ...)

(define-struct person [name hair eyes phone])
; Constructor
; String String String Number -> person
; (make-person ...)
(make-person "Frank" "Smith" "blue" 5551234567)

; Selectors
; (person-name ...)
; (person-hair ...)
; (person-eyes ...)
; (person-phone ...)
; Predicate
; (person? ...)

(define-struct pet [name number])
; Constructor
; String Number -> pet
; (make-pet ...)
(make-pet "Oodles" 234)

; Selectors
; (pet-name ...)
; (pet-number ...)
; Predicate
; (pet? ...)

(define-struct CD [artist title price])
; Constructor
; String String -> CD
; (make-CD ...)
(make-CD "Miles Davis" "Kind of Blue" 15)

; Selectors
; (CD-artist ...)
; (CD-title ...)
; (CD-price ...)
; Predicate
; (CD? ...)

; SweaterSize is one of the following Strings:
; - "s"
; - "m"
; - "l"
; - "xl"

(define-struct sweater [material size producer])
; Constructor
; String SweaterSize String -> sweater
; (make-sweater ...)
(make-sweater "cotton" "l" "sweatz")

; Selectors
; (sweater-material ...)
; (sweater-size ...)
; (sweater-producer ...)
; Predicate
; (sweater? ...)


