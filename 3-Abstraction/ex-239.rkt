;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-239) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A [List Number Number] is a structure:
;   (cons Number (cons Number '()))
(cons 3 (cons 4 '()))

; A [List Number 1String] is a structure:
;   (cons Number (cons 1String '()))
(cons 5 (cons "b" '()))

; A [List Number Boolean] is a structure:
;   (cons Number (cons Boolean '()))
(cons 8 (cons #false '()))
