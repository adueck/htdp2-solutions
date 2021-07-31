;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-388) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Employee is a structure
;  (make-employee String Number Number)
(define-struct employee [name ssn pay-rate])

; A WorkRecord is a structure
;  (make-work-record String Number)
(define-struct work-record [name hours])

; [List-of Employee] [List-of WorkRecord] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length
(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list (make-employee "Bill" 123 5.65)) (list (make-work-record "Bill" 40)))
              (list 226.0))
(check-expect (wages*.v2 (list (make-employee "Frank" 234 5.65) (make-employee "Jim" 456 8.75))
                         (list (make-work-record "Frank" 40.0) (make-work-record "Jim" 30.0)))
              '(226.0 262.5))
(define (wages*.v2 loe lowr)
  (map (lambda (e wr) (weekly-wage (employee-pay-rate e) (work-record-hours wr)))
       loe
       lowr))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))
