;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-335) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)

; a Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; a File* is one of:
; - '()
; - (cons File.v3 File*)

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String) 

(define dir1 (make-dir.v3
 "TS"
 (list (make-dir.v3
        "Text"
        '()
        (list (make-file "part1" 99 "")
              (make-file "part2" 52 "")
              (make-file "part3" 17 "")))
       (make-dir.v3
        "Libs"
        (list (make-dir.v3
               "Code"
               '()
               (list (make-file "hang" 8 "")
                     (make-file "draw" 2 "")))
              (make-dir.v3
               "Docs"
               '()
               (list (make-file "read!" 19 "")))
        '())
        '()))
 (list (make-file "read!" 10 ""))))
             
