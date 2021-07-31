;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-389) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; Combines a list of names as strings and a list of phone numbers
; as strings into a list of phone records
; [List-of String] [List-of String] -> [List-of PhoneRecord]
(check-expect (zip '() '()) '())
(check-expect (zip '("Bill" "Frank" "Joe")
                   '("123" "234" "345"))
              `(,(make-phone-record "Bill" "123")
                ,(make-phone-record "Frank" "234")
                ,(make-phone-record "Joe" "345")))
;(define (zip lon loph)
;  (cond
;    [(empty? lon) '()]
;    [else (cons
;            (make-phone-record (first lon) (first loph))
;            (zip (rest lon) (rest loph)))]))
(define (zip lon loph)
  (map (lambda (n ph) (make-phone-record n ph))
       lon
       loph))
