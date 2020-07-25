;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-058) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Price falls into one of three intervals: 
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item

; The percentage of tax charged on items
; 1000 through 10000 in value
(define LOW-TAX 0.05)
; The percentage of tax charged on items
; 10000 and above in value
(define LUXURY-TAX 0.08)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* LOW-TAX 1000))
(check-expect (sales-tax 12017) (* LUXURY-TAX 12017))

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p 1000)) 0]
    [(and (<= 1000 p) (< p 10000)) (* LOW-TAX p)]
    [(>= p 10000) (* LUXURY-TAX p)]))
