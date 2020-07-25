;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-078) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct tlw [l1 l2 l3])
; Tlw is a structure
;  (make-tlw Letter Letter Letter)
; interpretation a three letter word made up of
; three letters or blanks

; A Letter is one of:
; - LowercaseLetter
; - #false

; A LowercaseLetter is a 1String between "a" and "z"