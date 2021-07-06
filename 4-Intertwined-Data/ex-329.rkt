;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-329) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; a file named read! occurs twice in the directory tree
; TS
; occurance one: from TS root to the file
; occurance two: from TS root to Libs (DIR) to Docs (DIR) to the file

; the total size of all the files in the tree is
(define total-size-w-out-dirs (+ 10 99 52 17 19 8 2))
; which is 207

; there are 5 directories
(define total-size (+ (* 5 1) total-size-w-out-dirs))
; so the total size if each directory node has size 1
; is 212.

; it contains 3 levels of directories