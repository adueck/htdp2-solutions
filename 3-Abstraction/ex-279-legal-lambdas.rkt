;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-279-legal-lambdas) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(lambda (x y) (x y y))

; This is legal, you just have to pass a function for x

(lambda () 10)

; Illegal, needs at least one variable in the ()

(lambda (x) x)

; Legal, simply takes a variable and passes it back

(lambda (x y) x)

; Legal, not every variable needs to be used

(lambda x 10)

; Illeagal, x needs to be in ()

