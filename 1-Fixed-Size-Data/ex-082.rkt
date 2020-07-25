;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-082) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct tlw [l1 l2 l3])
; Tlw is a structure
;  (make-tlw Letter Letter Letter)
; interpretation a three letter word made up of
; three letters or blanks

; A Letter is one of:
; - LowercaseLetter
; - #false

; A LowercaseLetter is a 1String between "a" and "z"

; Tlw Tlw -> Tlw
; compares two given words
(check-expect (compare-words
               (make-tlw "a" "b" "c")
               (make-tlw "a" "b" "c"))
              (make-tlw "a" "b" "c"))
(check-expect (compare-words
               (make-tlw "d" "o" "g")
               (make-tlw "d" "o" "t"))
              (make-tlw "d" "o" #false))
(check-expect (compare-words
               (make-tlw "a" "t" #false)
               (make-tlw "a" "b" #false))
              (make-tlw "a" #false #false))
(define (compare-words w1 w2)
  (make-tlw
   (compare-letters (tlw-l1 w1) (tlw-l1 w2))
   (compare-letters (tlw-l2 w1) (tlw-l2 w2))
   (compare-letters (tlw-l3 w1) (tlw-l3 w2))))

; Letter Letter -> Letter
; returns the Letter if matching, or #false if doesn't match
(check-expect (compare-letters "a" "a") "a")
(check-expect (compare-letters "a" "b") #false)
(check-expect (compare-letters #false #false) #false)
(define (compare-letters l1 l2)
  (if (equal? l1 l2) l1 #false))