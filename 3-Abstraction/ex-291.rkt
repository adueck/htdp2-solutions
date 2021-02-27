;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-291) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (map-via-fold add1 (list 1 2 3)) (list 2 3 4))
(check-expect (map-via-fold (lambda (x) (string-append "Hi " x)) (list "Bob" "Jim"))
              (list "Hi Bob" "Hi Jim"))
; (X -> Z) [List-of X] -> [List-of Z]
(define (map-via-fold f l)
  (foldr
   (lambda (cur acc) (cons (f cur) acc))
   '()
   l))