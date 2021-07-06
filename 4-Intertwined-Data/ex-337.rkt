;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-337) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])

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
               (list (make-file "read!" 19 ""))))
        '()))
 (list (make-file "read!" 10 ""))))


; Dir.v3 -> Number
; determines how many files a given Dir.v3 contains
(check-expect (how-many dir1) 7)
(check-expect (how-many (make-dir.v3
                         "empty"
                         '()
                         '())) 0)
(define (how-many d)
  (local (; [List-of Dir.v3] -> Number
          (define (for-dirs ds)
            (foldl
             (lambda (dir n) (+ n (how-many dir)))
             0
             ds)))     
   (+ (for-dirs (dir.v3-dirs d))
      (length (dir.v3-files d)))))




             
