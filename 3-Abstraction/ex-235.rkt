;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-235) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(check-expect (contains? "foo" '())
              #false)
(check-expect (contains? "foo" '("foo" "bar"))
              #true)
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; determines whether l contains "atom"
(check-expect (contains-atom?
               '("a" "b" "atom"))
              #true)
(check-expect (contains-atom?
               '("a" "b" "c"))
              #false)
(check-expect (contains-atom? '())
              #false)
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; determines whether l contains "basic"
(check-expect (contains-basic?
               '("a" "b" "basic"))
              #true)
(check-expect (contains-basic?
               '("a" "b" "c"))
              #false)
(check-expect (contains-basic? '())
              #false)
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; determines whether l contains "zoo"
(check-expect (contains-zoo?
               '("a" "b" "zoo"))
              #true)
(check-expect (contains-zoo?
               '("a" "b" "c"))
              #false)
(check-expect (contains-zoo? '())
              #false)
(define (contains-zoo? l)
  (contains? "zoo" l))