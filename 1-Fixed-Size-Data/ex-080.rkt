;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-080) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title director year])

; Movie -> 1String
; returns the first letter of a movie's title
(define (movie-title-letter m)
  (
   ... (movie-title m) ...
   ... (movie-director m) ...
   ... (movie-year m) ...))

; Pet -> String
; returns a greeting for a pet based on its name
(define (greet-pet p)
  (... (pet-name p) ... (pet-number p) ...))

; CD -> String
; returns a title label based with the Artist and title info
(define (cd-label c)
  (... (CD-artist c) ... (CD-title c) ... (CD-price c) ...))

; Sweater -> String
; returns a detailed string describing the a Sweater's size
(define (size-detail s)
  (
   ... (sweater-material s) ...
   ... (sweater-size s) ...
   ... (sweater-color s) ...))
