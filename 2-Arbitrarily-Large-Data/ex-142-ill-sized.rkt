;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-142-ill-sized) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; ImageOrFalse is one of:
; – Image
; – #false

; List-of-images is one of:
; - '()
; - (cons Image List-of-images)

; List-of-images PositiveNumber -> ImageOrFalse
; returns the first image not fitting in a n sized
; square frame, if any
(check-expect (ill-sized? '() 20) #f)
(check-expect (ill-sized?
               (cons (square 20 "solid" "red") '()) 20)
              #f)
(check-expect (ill-sized?
               (cons (square 20 "solid" "red") '()) 19)
              (square 20 "solid" "red"))
(check-expect (ill-sized?
               (cons (square 20 "solid" "red")
                     (cons (square 19 "solid" "blue") '()))
               20)
              (square 19 "solid" "blue"))
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if
           (not (fits? (first loi) n))
           (first loi)
           (ill-sized? (rest loi) n))]))

; Image n -> Boolean
; determines if an image fits in a n sized square frame
(check-expect (fits? (square 20 "solid" "red") 20) #true)
(check-expect (fits? (square 20 "solid" "red") 21) #false)
(define (fits? i n)
  (and (= (image-height i) n)
       (= (image-width i) n)))