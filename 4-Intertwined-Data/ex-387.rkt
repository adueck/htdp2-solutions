;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-387) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Symbol] [List-of Number] -> [List of (cons Symbol (cons Number '())]
; consumes a list of symbols and a list of numbers and produces all possible
; ordered pairs of symbols and numbers.
(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross los lon)
  (local
    ; Symbol -> [List of (cons Symbol (cons Number '())]
    ((define (all-nums s)
       (map (lambda (n) (cons s (cons n '()))) lon)))
  (foldl
   (lambda (v ol) (append ol (all-nums v)))
   '()
   los)))