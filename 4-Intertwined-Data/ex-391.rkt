;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-391) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '(3 4) '()) '(3 4))
(check-expect (replace-eol-with '(1 2 3) '(4 5))
              '(1 2 3 4 5))
(check-expect (replace-eol-with '() '(1 2))
              '(1 2))
(define (replace-eol-with front end)
  (append front end))