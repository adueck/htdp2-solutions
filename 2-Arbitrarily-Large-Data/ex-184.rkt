;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-184) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect
 (list (string=? "a" "b") #false)
 (cons #f (cons #f '())))

(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (cons 30 (cons 200 (cons 0.5 '()))))

(check-expect
 (list "dana" "jane" "mary" "laura")
 (cons "dana" (cons "jane" (cons "mary" (cons "laura" '())))))