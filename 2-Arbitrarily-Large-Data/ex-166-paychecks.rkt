;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-166-paychecks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees
'()
(cons (make-work "Robby" 11.95 39)
      '())
(cons (make-work "Matthew" 12.95 45)
      (cons (make-work "Robby" 11.95 39)
            '()))
(cons (make-work "Phil" 15.10 20) '())
(cons (make-work "Fred" 11.99 35)
      (cons (make-work "Mark" 12.00 40) '()))

; Low -> List-of-numbers
; computes the weekly wages for the given records
(check-expect
  (wage*.v2
    (cons (make-work "Robby" 11.95 39) '()))
  (cons (* 11.95 39) '()))
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons (for-work (first an-low))
           (wage*.v2 (rest an-low)))]))

; Work -> Number
; computes the wage for the given work record w
(check-expect (for-work (make-work "Bob" 10 2)) 20)
(define (for-work w)
  (* (work-rate w) (work-hours w)))