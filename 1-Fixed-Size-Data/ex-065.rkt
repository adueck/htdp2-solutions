;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-065) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; Constructor
; (make-movie ...)
; Selectors
; (movie-title ...)
; (movie-producer ...)
; (movie-year ...)
; Predicate
; (movie? ...)

(define-struct person [name hair eyes phone])
; Constructor
; (make-person ...)
; Selectors
; (person-name ...)
; (person-hair ...)
; (person-eyes ...)
; (person-phone ...)
; Predicate
; (person? ...)

(define-struct pet [name number])
; Constructor
; (make-pet ...)
; Selectors
; (pet-name ...)
; (pet-number ...)
; Predicate
; (pet? ...)

(define-struct CD [artist title price])
; Constructor
; (make-CD ...)
; Selectors
; (CD-artist ...)
; (CD-title ...)
; (CD-price ...)
; Predicate
; (CD? ...)

(define-struct sweater [material size producer])
; Constructor
; (make-sweater ...)
; Selectors
; (sweater-material ...)
; (sweater-size ...)
; (sweater-producer ...)
; Predicate
; (sweater? ...)


