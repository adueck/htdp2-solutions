;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-452) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; interpretation represents the content of a file 
; "\n" is the newline character
(define NEWLINE "\n")

; File -> [List-of Line]
; converts a file into a list of lines 
(check-expect (file->list-of-lines
               (list "\n" "\n"))
               (list '()
                     '()))
(check-expect (file->list-of-lines
                (list "a" "b" "c" "\n"
                      "d" "e" "\n"
                      "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))
 
; File -> [List-of Line]
; converts a file into a list of lines 
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))

; File -> Line
; retrieves the prefix of afile up to the first occurrence of NEWLINE
(check-expect (first-line (list "a" "b" "\n" "c"))
              (list "a" "b"))
(check-expect (first-line '()) '())
(check-expect (first-line (list "a")) (list "a"))
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))

; File -> File
; drops the suffix of afile behind the first occurrence of NEWLINE
(check-expect (remove-first-line (list "a" "b" "\n" "c"))
              (list "c"))
(check-expect (remove-first-line '()) '())
(check-expect (remove-first-line (list "a")) '())
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))