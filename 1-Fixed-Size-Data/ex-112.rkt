;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-112) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? (make-posn 2 2)) #true)
(check-expect (missile-or-not? "text") #false)
(define (missile-or-not? v)
  (cond
    [(or (false? v) (posn? v)) #true]
    [else #false]))