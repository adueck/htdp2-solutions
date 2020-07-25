;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-185) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect
 (first (list 1 2 3))
 1)

(check-expect
 (rest (list 1 2 3))
 (cons 2 (cons 3 '())))

(check-expect
 (second (list 1 2 3))
 2)

; yes, third and fourth exist (as well as others too up to eigth)