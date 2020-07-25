;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-170-replace-phone-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

; A Lop (short for List-of-Phones) is one of the following
; - '()
; - (cons Phone Lop)
; interpration a list of phone numbers

; Lop -> Lop
; replaces all area code 713's with 281 in the list
(check-expect (replace '()) '())
(check-expect (replace
               (cons (make-phone 100 100 1000) '()))
              (cons (make-phone 100 100 1000) '()))
(check-expect (replace
               (cons (make-phone 713 100 1000) '()))
              (cons (make-phone 281 100 1000) '()))
(define (replace lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cons (r-num (first lop)) (replace (rest lop)))]))

; Phone -> Phone
; replaces the area code with 281 if it's 713
(check-expect (r-num (make-phone 100 100 1000))
              (make-phone 100 100 1000))
(check-expect (r-num (make-phone 713 100 1000))
              (make-phone 281 100 1000))
(define (r-num p)
  (make-phone
   (if (= (phone-area p) 713)
       281 (phone-area p))
   (phone-switch p)
   (phone-four p)))