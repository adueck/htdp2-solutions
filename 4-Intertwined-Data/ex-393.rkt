;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-393) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a [List-of Symbol] where every item is unique

; Produces the elements that exist in both of sets s1 and s2
; Set Set -> Set
(check-expect (union (list 'a 'b 'c) (list 'b 'c 'd))
              (list 'b 'c))
(check-expect (union '() '()) '())
(check-expect (union (list 'a 'b) (list 'f 'g)) '())
(define (union s1 s2)
  (cond
    [(empty? s1) '()]
    [else (if (member (first s1) s2)
              (cons
               (first s1)
               (union (rest s1) s2))
              (union (rest s1) s2))]))