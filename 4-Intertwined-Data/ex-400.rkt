;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-400) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A DNAB (DNA Base) is one of the following
; - 'a
; - 'c
; - 'g
; - 't

; A DD (DNA Description) is a [List-of DNAB]

(define dd1 (list 'a 'g 'c 'c))
(define dd2 (list 'g 'c 'a 'a 't))
(define dd3 (list 'a 'a))

; DD DD -> Booleas
; checks if p is the prefix of d
(check-expect (DNAprefix (list 'a 'g) dd1) #true)
(check-expect (DNAprefix (list 'c 't) dd3) #false)
(check-expect (DNAprefix '() dd2) #true)
(check-expect (DNAprefix (list 't) '()) #false)
(define (DNAprefix p d)
  (or
    (and
     (and (cons? p) (cons? d))
     (and (compare-dnab (first p) (first d))
          (DNAprefix (rest p) (rest d))))
    (empty? p)))

; DNAB DNAB -> Boolean
(check-expect (compare-dnab 'a 'a) #true)
(check-expect (compare-dnab 'g 't) #false)
(define (compare-dnab x y)
  (symbol=? x y))

; DD DD -> Maybe DNAB
(check-expect (DNAdelta (list 'a 'g) dd1) 'c)
(check-expect (DNAdelta (list 'c 't) dd3) #false)
(check-expect (DNAdelta '() dd3) (first dd3))
(check-expect (DNAdelta (list 'g 'g) '()) #false)
(check-error (DNAdelta dd1 dd1))
(check-error (DNAdelta '() '()))
(define (DNAdelta p d)
  (if (empty? p)
      ; reached end of prefix
      (if (cons? d) (first d) (error "nothing beyond match"))
      ; some prefix to check
      (if (and (cons? d) (compare-dnab (first p) (first d)))
          ; matched dnab - checking further
          (DNAdelta (rest p) (rest d))
          ; match unsuccesful or reached end of DD
          #false)))
    