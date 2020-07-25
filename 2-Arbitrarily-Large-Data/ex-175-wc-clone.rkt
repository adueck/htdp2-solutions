;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-175-wc-clone) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define sf (cons
                (cons "two" (cons "words" '()))
                (cons
                 (cons "three" '())
                 '())))
(define-struct count [lines words chars])
; A Count is a structure
;  (make-count Number Number Number)
; interpretation the amount of lines, words, and chars
; in a file

; String -> String
; outputs wc-style file info
(define (wc-clone f)
  (format-count (analyze-file (read-words/line f))))

; Count -> String
; outputs a Count in human readable format
(check-expect (format-count (make-count 1 2 3))
              "1 line(s), 2 word(s), 3 char(s)") 
(define (format-count c)
  (string-append
   (number->string (count-lines c))
   " line(s), "
   (number->string (count-words c))
   " word(s), "
   (number->string (count-chars c))
   " char(s)"))

; List-of-list-of-strings -> Count
; obtains the info about counts from a file
(check-expect (analyze-file '()) (make-count 0 0 0))
(check-expect (analyze-file sf) (make-count 2 3 13))
(define (analyze-file fi)
  (cond
    [(empty? fi) (make-count 0 0 0)]
    [(cons? fi) (increment-line
                 (add-counts
                  (analyze-line (first fi))
                  (analyze-file (rest fi))))]))

; List-of-strings -> Count
; obtains the count info for one line
(check-expect (analyze-line '()) (make-count 0 0 0))
(check-expect (analyze-line (cons "foo" (cons "bar" '())))
              (make-count 0 2 6))
(check-expect (analyze-line (cons "a" '()))
              (make-count 0 1 1))
(define (analyze-line los)
  (cond
    [(empty? los) (make-count 0 0 0)]
    [(cons? los) (add-counts
                  (make-count 0 1 (count-chrs (first los)))
                  (analyze-line (rest los)))]))

; Count -> Count
; increments the line number for a count
(check-expect (increment-line (make-count 0 0 0))
              (make-count 1 0 0))
(check-expect (increment-line (make-count 2 3 4))
              (make-count 3 3 4))
(define (increment-line c)
  (make-count
   (add1 (count-lines c))
   (count-words c)
   (count-chars c)))

; String -> Number
; obtains the number of chars in a string
(check-expect (count-chrs "") 0)
(check-expect (count-chrs "foo") 3)
(define (count-chrs s)
  (length (explode s)))

; Count -> Count
; adds two count structs together
(check-expect (add-counts (make-count 0 0 0)
                          (make-count 2 2 2))
              (make-count 2 2 2))
(check-expect (add-counts (make-count 1 2 3)
                          (make-count 2 3 4))
              (make-count 3 5 7))
(define (add-counts c1 c2)
  (make-count
   (+ (count-lines c1) (count-lines c2))
   (+ (count-words c1) (count-words c2))
   (+ (count-chars c1) (count-chars c2))))
