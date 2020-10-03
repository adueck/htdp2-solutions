;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-240) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

; An LNum is one of: 
; – Number
; – (make-layer LNum)

; An [LLayer] is one of:
; - X
; - (make-layer X)

; Instatiate to get back to the originals

; An LStrLayer is one of:
; - String
; - (make-layer String)

; An LNumLayer is one of:
; - Number
; - (make-layer Number)
