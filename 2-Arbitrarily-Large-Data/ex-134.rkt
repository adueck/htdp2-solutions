;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-134) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of: 
; – '()
; – (cons String List-of-strings)

; List-of-strings string -> Boolean
; determines whether s is in a list-of-strings
(check-expect (contains? (cons "bar" '()) "foo") #false)
(check-expect (contains? (cons "bar" '()) "bar") #true)
(check-expect (contains? (cons "foo"
                               (cons "bar" '())) "bar")
              #true)
(check-expect (contains? (cons "foo"
                               (cons "foo" '())) "bar")
              #false)
(define (contains? alos s)
  (cond
    [(empty? alos) #false]
    [(cons? alos)
     (or
      (string=? (first alos) s)
      (contains? (rest alos) s))]))

; this is the same as the 'or' example, because the
; (cond ... statement also will always evaluate
; to a #true or #false just like the 'or' expression


