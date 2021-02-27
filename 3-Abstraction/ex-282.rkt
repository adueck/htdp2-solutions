;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-282) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)
(define th 12)

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
     (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))


((lambda (ir) (<= (ir-price ir) th))
 (make-ir "bear" 10))
; #true