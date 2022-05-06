;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-423) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; String N -> [List-of String]
; Produces a list of string chunks from the string s of size n
(check-expect (partition "apple" 2) '("ap" "pl" "e"))
(check-expect (partition "apple" 5) '("apple"))
(check-expect (partition "" 3) '(""))
(define (partition s n)
  (cond
    [(<= (string-length s) n) (list (substring s 0))]
    [else
     (append
      (list (substring s 0 n))
      (partition (substring s n) n))]))
     