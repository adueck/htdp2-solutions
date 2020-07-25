;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ZooAnimal is one of the following:
; - Spider
; - Elephant
; - Boa
; - Armadillo

(define-struct cage [space transparent])
; A Cage is a structure
;  (make-cage Space Boolean)

(define-struct space [width height depth])
; A Space is a structure
; intepretation the width and height and depth that an animal takes up
;  (make-space Number Number Number)

(define-struct spider [legs space])
; A Spider is a structure:
;  (make-spider Number Space)

; An Elephant is a Space

(define-struct boa [length width])
; A Boa is a structure
;  (make-boa Number Number)

(define-struct armadillo [number space])
; An Armadillo is a structure
;  (make-armadillo Number Space)

; ZooAnimal Cage -> Boolean
; determines if a given ZooAnimal will fit in a given Cage
(check-expect
 (fits? (make-spider 3 (make-space 10 10 10))
        (make-cage (make-space 12 12 12) #true))
 #true)
(check-expect
 (fits? (make-spider 3 (make-space 10 10 10))
        (make-cage (make-space 12 8 12) #false))
 #false)
(check-expect
 (fits? (make-space 2 2 2) (make-cage (make-space 3 3 3) #true))
 #true)
(check-expect
 (fits? (make-boa 5 10) (make-cage (make-space 2 2 2) #true)) #false)
(check-expect
 (fits? (make-armadillo 25 (make-space 2 2 2))
        (make-cage (make-space 10 10 10) #false)) #true)
(define (fits? a c)
  (is-smaller?
   (cond
    [(spider? a) (spider-space a)]
    [(space? a) a]
    [(boa? a)
     (make-space
      (+ (space-height (cage-space c)) 1) ; cage-height irrelevant for snakes
      (boa-length a)
      (boa-width a))]
    [(armadillo? a) (armadillo-space a)])
   (cage-space c)))

; Space Space -> Boolean
; Determines if a given size is less than another
(check-expect (is-smaller?
               (make-space 2 2 2)
               (make-space 3 3 3))
              #true)
(check-expect (is-smaller?
               (make-space 3 4 3)
               (make-space 2 4 3))
              #false)
(define (is-smaller? inner outer)
  (and
   (< (space-height inner) (space-height outer))
   (< (space-width inner) (space-width outer))
   (< (space-depth inner) (space-depth outer))))

