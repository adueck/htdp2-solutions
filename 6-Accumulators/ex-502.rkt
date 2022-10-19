;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-502) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
  (palindrome (explode "abc")) (explode "abcba"))
(check-expect
  (palindrome (explode "a")) (explode "a"))
(define (palindrome s0)
  (local
    (; [List-of 1String] -> [List-of 1String]
     ; accumulater a is the letters in l to the penulitmate
     ; letter in s0
     (define (other-side l a)
       (cond
         [(<= (length l) 1) a]
         [else (other-side
                (rest l)
                (cons (first l) a))])))
  (append s0
          (other-side s0 '()))))

