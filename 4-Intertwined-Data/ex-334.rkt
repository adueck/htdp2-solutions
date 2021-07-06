;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-334) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
(define-struct dir [name content])
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String. 

(define dir1
  (make-dir "TS"
            (list (make-dir "Text" (list "part1" "part2" "part3"))
              "read!"
              (make-dir "Libs"
                        (list
                         (make-dir "Code" (list "hang" "draw"))
                         (make-dir "Docs" (list "read!")))))))
(define dir2
  (make-dir "base" (list
                     "README"
                     "License"
                     (make-dir "src" (list "index"))
                     )))

; Dir.v2 -> Number
; determines how many files a given Dir.v2 contains
(check-expect (how-many (make-dir "empty" '())) 0)
(check-expect (how-many dir1) 7)
(check-expect (how-many dir2) 3)
(define (how-many d)
  (for-lofd (dir-content d)))

; LOFD -> Number
; determines how many files a given LOFD contains
(define (for-lofd l)
  (cond
    [(empty? l) 0]
    [(string? (first l)) (add1 (for-lofd (rest l)))]
    [else (+
           (how-many (first l))
           (for-lofd (rest l)))]))

