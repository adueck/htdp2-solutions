;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-172-collapse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; List-of-list-of-strings -> String
; converts a list of lines into a string with \n for
; linebreaks
(check-expect (collapse '()) "")
(check-expect (collapse
               (cons
                (cons "first" (cons "line" '()))
                (cons
                 (cons "last" (cons "line" '())) '())))
              "first line\nlast line")
(check-expect (collapse
               (cons (cons "One" '()) (cons '() '())))
              "One\n")
(define (collapse lols)
  (cond
    [(empty? lols) ""]
    [(cons? lols)
     (string-append
      (make-line (first lols))
      (if (empty? (rest lols)) "" "\n")
      (collapse (rest lols)))]))

; List-of-strings -> String
; turns a list of words into a line of text
(check-expect (make-line '()) "")
(check-expect (make-line (cons "apple" (cons "bear" '())))
              "apple bear")
(define (make-line los)
  (cond
    [(empty? los) ""]
    [(cons? los)
     (string-append
      (first los)
      (if (empty? (rest los)) "" " ")
      (make-line (rest los)))]))
