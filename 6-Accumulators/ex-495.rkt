;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-495) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(sum.v2 '(10 4))
== (sum/a '(10 4) 0)
== (sum/a '(4) (+ 10 0))
== (sum/a '(4) 10)
== (sum/a '() (+ 4 10))
== (sum/a '() 14)
== 14
