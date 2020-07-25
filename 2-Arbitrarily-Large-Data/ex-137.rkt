;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-137) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-strings -> Number
; determines how many strings are on alos
(define (how-many alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (how-many (rest alos)) ...)]))


; List-of-names -> Boolean
(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ... (contains-flatt? (rest alon)) ...)]))


; Both deal with one atom of unstructured data per
; list item, and so the beginning template is the same
; becuase the level of complexity of the data is the same