;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-401) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define se1 5)
(define se2 (list 3 (list "a" 'c se1)))
(define se3 (list se1 se2))
(define se4 (list "a" se3 'c se2))

; Determines if two S-expr s are equal
; S-expr S-expr -> Boolean
(check-expect (sexp=? se1 se1) #true)
(check-expect (sexp=? se3 se3) #true)
(check-expect (sexp=? se2 se3) #false)
(check-expect (sexp=? se1 se3) #false)
(check-expect (sexp=? '() '()) #true)
(check-expect (sexp=? '() se3) #false)
(check-expect (sexp=? se1 '()) #false)
(check-expect (sexp=? "a" 'a) #false)
(check-expect (sexp=? se4 se4) #true)
(define (sexp=? s1 s2)
  (local
    ; Atom Atom -> Boolean
    ((define (compare-atom a1 a2)
      (cond
        [(and (number? a1) (number? a2)) (= a1 a2)]
        [(and (string? a1) (string? a2)) (string=? a1 a2)]
        [(and (symbol? a1) (symbol? a2)) (symbol=? a1 a2)]
        [else #false]))
    ; [List-of S-expr] [List-of S-expr] -> Boolean
    (define (compare-los l1 l2)
      (cond
       [(and (empty? l1) (empty? l2)) #true]
       [(and (cons? l1) (cons? l2))
        (and (sexp=? (first l1) (first l2))
             (compare-los (rest l1) (rest l2)))]
       [else #false])))
  (cond
    [(and (atom? s1) (atom? s2)) (compare-atom s1 s2)]
    [(and (los? s1) (los? s2)) (compare-los s1 s2)]
    [else #false])))

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

(define (los? x)
  (not (atom? x)))