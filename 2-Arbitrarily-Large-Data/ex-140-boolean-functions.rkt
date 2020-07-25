;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-140-boolean-funcitons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)

; List-of-booleans -> Boolean
; determines whether all items in a list are #true
(check-expect (all-true '()) #t)
(check-expect (all-true (cons #t '())) #t)
(check-expect (all-true (cons #f (cons #t '())))
              #f)
(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [else (and
           (first lob)
           (all-true (rest lob)))]))

; List-of-booleans -> Boolean
; determines if one item in a list is #true
(check-expect (one-true '()) #f)
(check-expect (one-true (cons #t '())) #t)
(check-expect (one-true (cons #f (cons #t '()))) #t)
(check-expect (one-true (cons #f (cons #f '()))) #f)
(define (one-true lob)
  (cond
    [(empty? lob) #f]
    [else (or
           (first lob)
            (one-true (rest lob)))]))