;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-147-non-empty-boolean-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)

; NEList-of-booleans -> Boolean
; determines whether all items in a list are #true
(check-expect (all-true (cons #t '())) #t)
(check-expect (all-true (cons #f '())) #f)
(check-expect (all-true (cons #f (cons #t '())))
              #f)
(check-expect (all-true (cons #t (cons #t '())))
              #t)
(define (all-true ne-lob)
  (cond
    [(empty? (rest ne-lob)) (first ne-lob)]
    [else (and
           (first ne-lob)
           (all-true (rest ne-lob)))]))

; NEList-of-booleans -> Boolean
; determines if one item in a list is #true
(check-expect (one-true (cons #t '())) #t)
(check-expect (one-true (cons #f '())) #f)
(check-expect (one-true (cons #f (cons #t '()))) #t)
(check-expect (one-true (cons #f (cons #f '()))) #f)
(define (one-true ne-lob)
  (cond
    [(empty? (rest ne-lob)) (first ne-lob)]
    [else (or
           (first ne-lob)
            (one-true (rest ne-lob)))]))