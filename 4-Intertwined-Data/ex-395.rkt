;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-395) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] Number -> [List-of X]
; produces the first n items from l or all of l if it is too short
(check-expect (take '(3 7 9) 2) '(3 7))
(check-expect (take '() 5) '())
(check-expect (take '(5 9) 10) '(5 9))
(check-expect (take '(2 2 4 5) 3) '(2 2 4))
(check-expect (take '() 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(define (take l n)
  (cond
    [(and (> n 0) (cons? l))
     (cons (first l) (take (rest l) (sub1 n)))]
    [(= n 0) '()]
    [else l]))

; [List-of X] Number -> [List of X]
; produces l with the first n items removed or just ’() if l is too short
(check-expect (drop '(2 3 4) 2) '(4))
(check-expect (drop '(3 5 7) 4) '())
(check-expect (drop '() 0) '())
(check-expect (drop '() 10) '())
(define (drop l n)
  (if (and (> n 0) (cons? l))
      (drop (rest l) (sub1 n))
      l))
