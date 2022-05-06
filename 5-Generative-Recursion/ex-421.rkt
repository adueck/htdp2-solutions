;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-421) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define s1 (list "a" "b" "c" "d" "e" "f" "g" "h"))
(define b2 (list "ab" "cd" "ef" "gh"))
(define b3 (list "abc" "def" "gh"))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle s1 2) b2)
(check-expect (bundle s1 3) b3)
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(check-expect (take '("a" "b" "c") 2) '("a" "b"))
(check-expect (take '("a" "b" "c") 5) '("a" "b" "c"))
(check-expect (take '() 10) '())
(check-expect (take '("a" "b") 0) '())
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(check-expect (drop '("a" "b" "c") 1) '("b" "c"))
(check-expect (drop '("a" "b" "c") 5) '())
(check-expect (drop '() 2) '())
(check-expect (drop '("a" "b") 0) '("a" "b"))
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) '()]
    [else (drop (rest l) (sub1 n))]))

; (bundle '("a" "b" "c") 0) is not a proper use of the bundle function.
; It produces an infinite recursion and stack overflow becaues there is
; no base case in the function the list cannot be moved along because
; there are essetially no chunks to move along/process with
  