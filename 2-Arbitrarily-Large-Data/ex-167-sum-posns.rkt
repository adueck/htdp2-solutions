;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-167-sum-posns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee [name number])
; An Employee is a structure
;  (make-employee String Number)
; interpretation cobines an employees name and number

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work Employee Number Number)
; interpretation (make-work e r h) combines the employee info e 
; with the pay rate r and the number of hours h

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
;   (make-paycheck Employee Number)
; interpretation (make-paycheck e a) combines
; the employee info e and the amount a they get paid

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees
'()
(cons (make-work (make-employee "Robby" 1) 11.95 39)
      '())
(cons (make-work (make-employee "Matthew" 2) 12.95 45)
      (cons (make-work (make-employee "Robby" 1) 11.95 39)
            '()))

; Lop (short for list of paychecks) is one of: 
; – '()
; – (cons Paycheck Lop)
; interpretation an instance of Lop represents the 
; the employees name and the amount they are paid

; Work -> Number
; computes the wage for the given work record w
(check-expect (for-work (make-work
                         (make-employee "Bob" 1) 10 2)) 20)
(define (for-work w)
  (* (work-rate w) (work-hours w)))

; Low -> Lop
; Produces a list of paychecks to be paid out from
; a list of work
(check-expect (wage*.v4 '()) '())
(check-expect (wage*.v4
               (cons (make-work
                      (make-employee "Robby" 1) 11.95 39) '()))
              (cons (make-paycheck
                     (make-employee "Robby" 1) (* 11.95 39)) '()))
(define (wage*.v4 low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (calc-paycheck (first low))
          (wage*.v4 (rest low)))]))

; Work -> Paycheck
; Produces a paycheck for an item of work
(check-expect (calc-paycheck
               (make-work (make-employee "Bob" 2) 10 20))
              (make-paycheck (make-employee "Bob" 2) 200))
(define (calc-paycheck w)
  (make-paycheck
   (work-employee w) 
   (* (work-hours w) (work-rate w))))