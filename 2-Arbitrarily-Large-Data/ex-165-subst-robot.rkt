;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-165-subst-robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-toy-descriptions is a List-of-strings
; interpretation each string is a one-word description
; for a toy

; List-of-toy-descriptions -> List-of-toy-descriptions
; replaces all occurances of "robot" with "r2d2" in lotd
(check-expect (subst-robot '()) '())
(check-expect (subst-robot
               (cons "duck" (cons "robot" (cons "robot" '()))))
               (cons "duck" (cons "r2d2" (cons "r2d2" '()))))
(define (subst-robot lotd)
  (cond
    [(empty? lotd) '()]
    [else
     (cons
      (if (string=? (first lotd) "robot")
          "r2d2"
          (first lotd)) (subst-robot (rest lotd)))]))

; List-of-strings String String -> List-of-strings
; substitutes all occurances of old with new
(check-expect (substitute '() "foo" "bar") '())
(check-expect (substitute (cons "foo" '()) "foo" "bar")
              (cons "bar" '()))
(check-expect (substitute (cons "thing" '()) "foo" "bar")
              (cons "thing" '()))
(define (substitute los old new)
  (cond
    [(empty? los) '()]
    [else (cons
           (if (string=? (first los) old)
               new (first los))
               (substitute (rest los) old new))]))